module Grain.UI.Prop
  ( allocProps
  , updateProps
  ) where

import Prelude

import Control.Safely (for_)
import Data.Array (filter, notElem, snoc, union)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Tuple (Tuple, uncurry)
import Effect (Effect)
import Foreign (unsafeToForeign)
import Foreign.Object (Object, keys, lookup, toUnfoldable)
import Grain.UI.Util (classNames, hasXlinkPrefix, isBoolean, isProperty, removeAttributeNS_, setAttributeNS_, setForeign)
import Grain.Styler (Styler, registerStyle)
import Web.DOM.DOMTokenList as DTL
import Web.DOM.Element (Element, classList, getAttribute, removeAttribute, setAttribute)

allocProps
  :: Styler
  -> Boolean
  -> Object String
  -> Element
  -> Effect Unit
allocProps styler isSvg props el =
  let props' :: Array (Tuple String String)
      props' = toUnfoldable props
   in for_ props' $ uncurry \name val -> setProp styler isSvg name val el

updateProps
  :: Styler
  -> Boolean
  -> Object String
  -> Object String
  -> Element
  -> Effect Unit
updateProps styler isSvg currents nexts el =
  for_ names updateByName
  where
    names = union (keys currents) (keys nexts)
    updateByName name =
      case lookup name currents, lookup name nexts of
        Nothing, Nothing -> pure unit
        Just c, Nothing -> removeProp isSvg name c el
        Nothing, Just n -> setProp styler isSvg name n el
        Just c, Just n
          | c == n -> pure unit
          | notElem name [ "class", "className" ] -> setProp styler isSvg name n el
          | otherwise ->
              let currentClasses = classNames c
                  nextClasses = classNames n
                  removeTargets = filter (flip notElem nextClasses) currentClasses
                  addTargets = filter (flip notElem currentClasses) nextClasses
               in do
                  removeProp isSvg name (joinWith " " removeTargets) el
                  setProp styler isSvg name (joinWith " " addTargets) el

setProp
  :: Styler
  -> Boolean
  -> String
  -> String
  -> Element
  -> Effect Unit
setProp styler isSvg "css" val el = do
  generatedClassName <- registerStyle val styler
  removeProp isSvg "css" "dummyVal" el
  setStylerAttribute generatedClassName el
  addClass isSvg generatedClassName el
setProp styler isSvg "class" val el =
  setProp styler isSvg "className" val el
setProp styler isSvg "className" val el =
  for_ (classNames val) $ flip (addClass isSvg) el
setProp _ _ "style" val el =
  setAttribute "style" val el
setProp _ _ "list" val el =
  setAttribute "list" val el
setProp _ _ "form" val el =
  setAttribute "form" val el
setProp _ _ "dropzone" val el =
  setAttribute "dropzone" val el
setProp _ isSvg name val el =
  if isProperty name el && not isSvg
    then
      if isBoolean name el
        then setForeign name (unsafeToForeign $ not $ val == "false") el
        else setForeign name (unsafeToForeign val) el
    else
      if isSvg && hasXlinkPrefix name
        then setAttributeNS_ name val el
        else setAttribute name val el

removeProp
  :: Boolean
  -> String
  -> String
  -> Element
  -> Effect Unit
removeProp isSvg "css" _ el = do
  maybeClassName <- getStylerAttribute el
  case maybeClassName of
    Nothing -> pure unit
    Just generatedClassName ->
      removeClass isSvg generatedClassName el
removeProp isSvg "class" val el =
  removeProp isSvg "className" val el
removeProp isSvg "className" val el =
  for_ (classNames val) $ flip (removeClass isSvg) el
removeProp _ "style" _ el =
  removeAttribute "style" el
removeProp _ "list" _ el =
  removeAttribute "list" el
removeProp _ "form" _ el =
  removeAttribute "form" el
removeProp _ "dropzone" _ el =
  removeAttribute "dropzone" el
removeProp isSvg name _ el = do
  if isSvg && hasXlinkPrefix name
    then removeAttributeNS_ name el
    else do
      when (isProperty name el && not isSvg)
        $ setForeign name (unsafeToForeign "") el
      removeAttribute name el

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
