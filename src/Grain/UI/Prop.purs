module Grain.UI.Prop
  ( allocProps
  , updateProps
  ) where

import Prelude

import Data.Array (filter, notElem, snoc, union)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect (Effect)
import Foreign.Object (Object, keys, lookup)
import Grain.Effect (forObjectE, foreachE)
import Grain.Styler (Styler, registerStyle)
import Grain.UI.Util (classNames, hasXlinkPrefix, isBoolean, isProperty, removeAttributeNS_, setAny, setAttributeNS_)
import Web.DOM.DOMTokenList as DTL
import Web.DOM.Element (Element, classList, getAttribute, removeAttribute, setAttribute)

type AllocArgs =
  { isSvg :: Boolean
  , styler :: Styler
  , nexts :: Object String
  , element :: Element
  }

allocProps :: AllocArgs -> Effect Unit
allocProps args =
  forObjectE args.nexts \name val ->
    setProp
      { isSvg: args.isSvg
      , styler: args.styler
      , name
      , val
      , element: args.element
      }

type UpdateArgs =
  { isSvg :: Boolean
  , styler :: Styler
  , currents :: Object String
  , nexts :: Object String
  , element :: Element
  }

updateProps :: UpdateArgs -> Effect Unit
updateProps args =
  foreachE names updateByName
  where
    names = union (keys args.currents) (keys args.nexts)
    updateByName name =
      case lookup name args.currents, lookup name args.nexts of
        Nothing, Nothing ->
          pure unit
        Just c, Nothing ->
          removeProp
            { isSvg: args.isSvg
            , name
            , val: c
            , element: args.element
            }
        Nothing, Just n ->
          setProp
            { isSvg: args.isSvg
            , styler: args.styler
            , name
            , val: n
            , element: args.element
            }
        Just c, Just n
          | c == n ->
              pure unit
          | notElem name [ "class", "className" ] ->
              setProp
                { isSvg: args.isSvg
                , styler: args.styler
                , name
                , val: n
                , element: args.element
                }
          | otherwise ->
              let currentClasses = classNames c
                  nextClasses = classNames n
                  removeTargets = filter (flip notElem nextClasses) currentClasses
                  addTargets = filter (flip notElem currentClasses) nextClasses
               in do
                  removeProp
                    { isSvg: args.isSvg
                    , name
                    , val: joinWith " " removeTargets
                    , element: args.element
                    }
                  setProp
                    { isSvg: args.isSvg
                    , styler: args.styler
                    , name
                    , val: joinWith " " addTargets
                    , element: args.element
                    }

type SetArgs =
  { isSvg :: Boolean
  , styler :: Styler
  , name :: String
  , val :: String
  , element :: Element
  }

setProp :: SetArgs -> Effect Unit
setProp args =
  case args.name of
    "css" -> do
      generatedClassName <- registerStyle args.val args.styler
      removeProp
        { isSvg: args.isSvg
        , name: "css"
        , val: "dummyVal"
        , element: args.element
        }
      setStylerAttribute generatedClassName args.element
      addClass args.isSvg generatedClassName args.element
    "class" ->
      setProp args { name = "className" }
    "className" ->
      foreachE (classNames args.val) do
        flip (addClass args.isSvg) args.element
    "style" ->
      setAttribute "style" args.val args.element
    "list" ->
      setAttribute "list" args.val args.element
    "form" ->
      setAttribute "form" args.val args.element
    "dropzone" ->
      setAttribute "dropzone" args.val args.element
    _ ->
      if isProperty args.name args.element && not args.isSvg
        then
          if isBoolean args.name args.element
            then setAny args.name (args.val /= "false") args.element
            else setAny args.name args.val args.element
        else
          if args.isSvg && hasXlinkPrefix args.name
            then setAttributeNS_ args.name args.val args.element
            else setAttribute args.name args.val args.element

type RemoveArgs =
  { isSvg :: Boolean
  , name :: String
  , val :: String
  , element :: Element
  }

removeProp :: RemoveArgs -> Effect Unit
removeProp args =
  case args.name of
    "css" -> do
      maybeClassName <- getStylerAttribute args.element
      case maybeClassName of
        Nothing -> pure unit
        Just generatedClassName ->
          removeClass args.isSvg generatedClassName args.element
    "class" ->
      removeProp args { name = "className" }
    "className" ->
      foreachE (classNames args.val) do
        flip (removeClass args.isSvg) args.element
    "style" ->
      removeAttribute "style" args.element
    "list" ->
      removeAttribute "list" args.element
    "form" ->
      removeAttribute "form" args.element
    "dropzone" ->
      removeAttribute "dropzone" args.element
    _ ->
      if args.isSvg && hasXlinkPrefix args.name
        then removeAttributeNS_ args.name args.element
        else do
          when (isProperty args.name args.element && not args.isSvg) do
            setAny args.name "" args.element
          removeAttribute args.name args.element

addClass :: Boolean -> String -> Element -> Effect Unit
addClass isSvg = if isSvg then addSvgClass else addStandardClass

addStandardClass :: String -> Element -> Effect Unit
addStandardClass val el = classList el >>= flip DTL.add val

addSvgClass :: String -> Element -> Effect Unit
addSvgClass val el = do
  maybeClassStr <- getAttribute "class" el
  case maybeClassStr of
    Nothing ->
      setAttribute "class" val el
    Just current ->
      let next = joinWith " " $ snoc (classNames current) val
       in setAttribute "class" next el

removeClass :: Boolean -> String -> Element -> Effect Unit
removeClass isSvg = if isSvg then removeSvgClass else removeStandardClass

removeStandardClass :: String -> Element -> Effect Unit
removeStandardClass val el = classList el >>= flip DTL.remove val

removeSvgClass :: String -> Element -> Effect Unit
removeSvgClass val el = do
  maybeClassStr <- getAttribute "class" el
  case maybeClassStr of
    Nothing -> pure unit
    Just current ->
      let next = joinWith " " $ filter (_ /= val) $ classNames current
       in setAttribute "class" next el

getStylerAttribute :: Element -> Effect (Maybe String)
getStylerAttribute = getAttribute stylerAttributeName

setStylerAttribute :: String -> Element -> Effect Unit
setStylerAttribute = setAttribute stylerAttributeName

stylerAttributeName :: String
stylerAttributeName = "data-grain-styler-class"
