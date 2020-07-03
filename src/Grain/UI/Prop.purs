module Grain.UI.Prop
  ( allocProps
  , updateProps
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Grain.Effect (foreachE)
import Grain.Styler (Styler, registerStyle)
import Grain.UI.PropDiff (PatchArgs(..), diff)
import Grain.UI.Util (hasXlinkPrefix, isBoolean, isProperty, removeAttributeNS_, setAny, setAttributeNS_)
import Web.DOM.Element (Element, removeAttribute, setAttribute)

type SpecialProps =
  { css :: Maybe String
  , className :: Maybe String
  }

type AllocArgs =
  { isSvg :: Boolean
  , styler :: Styler
  , nexts :: Array (Tuple String String)
  , specialNexts :: SpecialProps
  , element :: Element
  }

allocProps :: AllocArgs -> Effect Unit
allocProps args = do
  allocClassName args
  foreachE args.nexts \(Tuple name val) ->
    setProp
      { isSvg: args.isSvg
      , name
      , val
      , element: args.element
      }

type UpdateArgs =
  { isSvg :: Boolean
  , styler :: Styler
  , currents :: Array (Tuple String String)
  , nexts :: Array (Tuple String String)
  , specialCurrents :: SpecialProps
  , specialNexts :: SpecialProps
  , element :: Element
  }

updateProps :: UpdateArgs -> Effect Unit
updateProps args = do
  updateClassName args
  diff (patch args)
    { currents: args.currents
    , nexts: args.nexts
    }

patch
  :: UpdateArgs
  -> PatchArgs String
  -> Effect Unit
patch args (Create { next: Tuple name n }) =
  setProp
    { isSvg: args.isSvg
    , name
    , val: n
    , element: args.element
    }
patch args (Delete { current: Tuple name _ }) =
  removeProp
    { isSvg: args.isSvg
    , name
    , element: args.element
    }
patch args (Update { current: Tuple _ c, next: Tuple name n }) =
  when (c /= n) do
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

allocClassName :: AllocArgs -> Effect Unit
allocClassName args = do
  mn <- getClassName args.styler args.specialNexts
  case mn of
    Nothing ->
      pure unit
    Just n ->
      if args.isSvg
        then setAttribute "class" n args.element
        else setAny "className" n args.element

updateClassName :: UpdateArgs -> Effect Unit
updateClassName args = do
  mc <- getClassName args.styler args.specialCurrents
  mn <- getClassName args.styler args.specialNexts
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

getClassName :: Styler -> SpecialProps -> Effect (Maybe String)
getClassName styler specials =
  case specials.css, specials.className of
    Nothing, Nothing ->
      pure Nothing
    Nothing, Just cls ->
      pure $ Just cls
    Just css, Nothing ->
      Just <$> registerStyle css styler
    Just css, Just cls -> do
      cls' <- registerStyle css styler
      pure $ Just $ cls' <> " " <> cls
