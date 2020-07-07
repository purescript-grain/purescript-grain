module Grain.Internal.SpecialProp
  ( SpecialProps
  , allocSpecialProps
  , updateSpecialProps
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Uncurried as EFn
import Grain.Internal.Styler (Styler, registerStyle)
import Grain.Internal.Util (removeAttribute, setAny, setAttribute, whenE)
import Web.DOM.Element (Element)

type SpecialProps =
  { css :: Maybe String
  , className :: Maybe String
  }

allocSpecialProps
  :: EFn.EffectFn4 Boolean Styler SpecialProps Element Unit
allocSpecialProps = EFn.mkEffectFn4 \isSvg styler nexts element -> do
  mn <- EFn.runEffectFn2 getClassName styler nexts
  case mn of
    Just val ->
      EFn.runEffectFn3 setClassName isSvg val element
    _ ->
      pure unit

updateSpecialProps
  :: EFn.EffectFn5 Boolean Styler SpecialProps SpecialProps Element Unit
updateSpecialProps = EFn.mkEffectFn5 \isSvg styler currents nexts element -> do
  mc <- EFn.runEffectFn2 getClassName styler currents
  mn <- EFn.runEffectFn2 getClassName styler nexts
  case mc, mn of
    Just c, Just n ->
      EFn.runEffectFn2 whenE (c /= n) (EFn.runEffectFn3 setClassName isSvg n element)
    Nothing, Just val ->
      EFn.runEffectFn3 setClassName isSvg val element
    Just _, Nothing ->
      if isSvg
        then EFn.runEffectFn2 removeAttribute "class" element
        else EFn.runEffectFn3 setAny "className" "" element
    _, _ ->
      pure unit

setClassName :: EFn.EffectFn3 Boolean String Element Unit
setClassName = EFn.mkEffectFn3 \isSvg val element ->
  if isSvg
    then EFn.runEffectFn3 setAttribute "class" val element
    else EFn.runEffectFn3 setAny "className" val element

getClassName :: EFn.EffectFn2 Styler SpecialProps (Maybe String)
getClassName = EFn.mkEffectFn2 \styler specials ->
  case specials.css, specials.className of
    Nothing, Just cls ->
      pure $ Just cls
    Just css, Nothing -> do
      cls' <- EFn.runEffectFn2 registerStyle css styler
      pure $ Just cls'
    Just css, Just cls -> do
      cls' <- EFn.runEffectFn2 registerStyle css styler
      pure $ Just $ cls' <> " " <> cls
    _, _ ->
      pure Nothing
