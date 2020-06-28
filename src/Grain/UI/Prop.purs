module Grain.UI.Prop
  ( allocProps
  , updateProps
  ) where

import Prelude

import Data.Array (union)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign.Object (Object, delete, keys, lookup)
import Grain.Effect (forObjectE, foreachE)
import Grain.Styler (Styler, registerStyle)
import Grain.UI.Util (hasXlinkPrefix, isBoolean, isProperty, removeAttributeNS_, setAny, setAttributeNS_)
import Web.DOM.Element (Element, removeAttribute, setAttribute)

type AllocArgs =
  { isSvg :: Boolean
  , styler :: Styler
  , nexts :: Object String
  , element :: Element
  }

allocProps :: AllocArgs -> Effect Unit
allocProps args = do
  args' <- allocClassName args
  forObjectE args'.nexts \name val ->
    setProp
      { isSvg: args'.isSvg
      , name
      , val
      , element: args'.element
      }

type UpdateArgs =
  { isSvg :: Boolean
  , styler :: Styler
  , currents :: Object String
  , nexts :: Object String
  , element :: Element
  }

updateProps :: UpdateArgs -> Effect Unit
updateProps args = do
  args' <- updateClassName args
  let names = union (keys args'.currents) (keys args'.nexts)
  foreachE names $ updateByName args'

updateByName :: UpdateArgs -> String -> Effect Unit
updateByName args name =
  case lookup name args.currents, lookup name args.nexts of
    Nothing, Nothing ->
      pure unit
    Just _, Nothing ->
      removeProp
        { isSvg: args.isSvg
        , name
        , element: args.element
        }
    Nothing, Just n ->
      setProp
        { isSvg: args.isSvg
        , name
        , val: n
        , element: args.element
        }
    Just c, Just n
      | c == n ->
          pure unit
      | otherwise ->
          setProp
            { isSvg: args.isSvg
            , name
            , val: n
            , element: args.element
            }

type SetArgs =
  { isSvg :: Boolean
  , name :: String
  , val :: String
  , element :: Element
  }

setProp :: SetArgs -> Effect Unit
setProp args =
  case args.name of
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
  , element :: Element
  }

removeProp :: RemoveArgs -> Effect Unit
removeProp args =
  case args.name of
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

allocClassName :: AllocArgs -> Effect AllocArgs
allocClassName args = do
  mn <- getClassName args.styler args.nexts
  case mn of
    Nothing ->
      pure unit
    Just n ->
      if args.isSvg
        then setAttribute "class" n args.element
        else setAny "className" n args.element
  pure args
    { nexts = delete "css" $ delete "className" args.nexts
    }

updateClassName :: UpdateArgs -> Effect UpdateArgs
updateClassName args = do
  mc <- getClassName args.styler args.currents
  mn <- getClassName args.styler args.nexts
  case mc, mn of
    Nothing, Nothing ->
      pure unit
    Just _, Nothing -> do
      when (not args.isSvg) do
        setAny "className" "" args.element
      removeAttribute "class" args.element
    _, Just n ->
      if args.isSvg
        then setAttribute "class" n args.element
        else setAny "className" n args.element
  pure args
    { currents = delete "css" $ delete "className" args.currents
    , nexts = delete "css" $ delete "className" args.nexts
    }

getClassName :: Styler -> Object String -> Effect (Maybe String)
getClassName styler props =
  case lookup "css" props, lookup "className" props of
    Nothing, Nothing ->
      pure Nothing
    Nothing, Just cls ->
      pure $ Just cls
    Just css, Nothing ->
      Just <$> registerStyle css styler
    Just css, Just cls -> do
      cls' <- registerStyle css styler
      pure $ Just $ cls' <> " " <> cls
