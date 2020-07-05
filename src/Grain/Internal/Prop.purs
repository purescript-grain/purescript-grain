module Grain.Internal.Prop
  ( Props
  , allocProps
  , updateProps
  ) where

import Prelude

import Data.Array (elemIndex)
import Data.Function.Uncurried as Fn
import Data.Maybe (isJust)
import Data.Tuple (Tuple(..))
import Effect.Uncurried as EFn
import Grain.Internal.Effect (foreachE, whenE)
import Grain.Internal.PropDiff (PatchArgs(..), diff)
import Grain.Internal.Util (isBoolean, isProperty, removeAttribute, setAny, setAttribute)
import Web.DOM.Element (Element)

type Props =
  Array (Tuple String String)

allocProps
  :: EFn.EffectFn3 Boolean Props Element Unit
allocProps = EFn.mkEffectFn3 \isSvg nexts element ->
  EFn.runEffectFn2 foreachE nexts $ EFn.mkEffectFn1 \(Tuple name val) ->
    EFn.runEffectFn4 setProp isSvg name val element

updateProps
  :: EFn.EffectFn4 Boolean Props Props Element Unit
updateProps = EFn.mkEffectFn4 \isSvg currents nexts element ->
  EFn.runEffectFn2 diff (patch isSvg element) { currents, nexts }

patch
  :: Boolean
  -> Element
  -> EFn.EffectFn1 (PatchArgs String) Unit
patch isSvg element = EFn.mkEffectFn1 \act ->
  case act of
    Create { next: Tuple name val } ->
      EFn.runEffectFn4 setProp isSvg name val element
    Delete { current: Tuple name _ } ->
      EFn.runEffectFn3 removeProp isSvg name element
    Update { current: Tuple _ c, next: Tuple name n } ->
      EFn.runEffectFn2 whenE (c /= n) (EFn.runEffectFn4 setProp isSvg name n element)

setProp :: EFn.EffectFn4 Boolean String String Element Unit
setProp = EFn.mkEffectFn4 \isSvg name val element ->
  if isSvg || shouldAttribute name || not (Fn.runFn2 isProperty name element)
    then EFn.runEffectFn3 setAttribute name val element
    else
      if Fn.runFn2 isBoolean name element
        then EFn.runEffectFn3 setAny name (val /= "false") element
        else EFn.runEffectFn3 setAny name val element

removeProp :: EFn.EffectFn3 Boolean String Element Unit
removeProp = EFn.mkEffectFn3 \isSvg name element ->
  if isSvg || shouldAttribute name || not (Fn.runFn2 isProperty name element)
    then EFn.runEffectFn2 removeAttribute name element
    else EFn.runEffectFn3 setAny name "" element

shouldAttribute :: String -> Boolean
shouldAttribute name =
  isJust $ elemIndex name attributes

attributes :: Array String
attributes =
  [ "style"
  , "list"
  , "form"
  , "dropzone"
  ]
