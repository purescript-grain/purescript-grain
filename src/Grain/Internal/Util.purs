module Grain.Internal.Util
  ( Step
  , nonNull
  , byIdx
  , byIdxNullable
  , keyNullable
  , mapNullable
  , eqNullable
  , shouldAttribute
  , raf
  , head
  , nodeIndexOf
  , createTextNode
  , createElement
  , createElementNS
  , unsafeParentNode
  , appendChild
  , removeChild
  , replaceChild
  , putChild
  , setTextContent
  , setAny
  , setAttribute
  , removeAttribute
  , isProperty
  , isBoolean
  , mkEventListener
  , whenE
  , forE
  , foreachE
  , sequenceE
  , tailRecE
  ) where

import Prelude

import Data.Function.Uncurried as Fn
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried as EFn
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element)
import Web.DOM.Node (Node)
import Web.DOM.Text (Text)
import Web.Event.Event (Event)
import Web.Event.EventTarget (EventListener)

type Step r = { done :: Boolean | r }

nonNull :: forall a. Nullable a -> a
nonNull = unsafeCoerce

foreign import byIdx :: forall a. Fn.Fn2 (Array a) Int a
foreign import byIdxNullable :: forall a. Fn.Fn2 (Array a) Int (Nullable a)
foreign import keyNullable :: forall a. Fn.Fn3 (Fn.Fn2 Int a String) Int (Nullable a) (Nullable String)
foreign import mapNullable :: forall a b. Fn.Fn2 (a -> b) (Nullable a) (Nullable b)
foreign import eqNullable :: forall a. Fn.Fn2 (Nullable a) (Nullable a) Boolean
foreign import shouldAttribute :: String -> Boolean
foreign import raf :: EFn.EffectFn1 (Effect Unit) Unit
foreign import head :: Effect Node
foreign import createTextNode :: EFn.EffectFn1 String Text
foreign import createElement :: EFn.EffectFn1 String Element
foreign import createElementNS :: EFn.EffectFn1 String Element
foreign import unsafeParentNode :: EFn.EffectFn1 Node Node
foreign import appendChild :: EFn.EffectFn2 Node Node Node
foreign import removeChild :: EFn.EffectFn2 Node Node Unit
foreign import replaceChild :: EFn.EffectFn3 Node Node Node Unit
foreign import putChild :: EFn.EffectFn3 Int Node Node Unit
foreign import nodeIndexOf :: EFn.EffectFn1 Node Int
foreign import setTextContent :: EFn.EffectFn2 String Node Unit
foreign import setAny :: forall a. EFn.EffectFn3 String a Element Unit
foreign import setAttribute :: EFn.EffectFn3 String String Element Unit
foreign import removeAttribute :: EFn.EffectFn2 String Element Unit
foreign import isProperty :: Fn.Fn2 String Element Boolean
foreign import isBoolean :: Fn.Fn2 String Element Boolean
foreign import mkEventListener :: (Event -> Effect Unit) -> EventListener
foreign import whenE :: EFn.EffectFn2 Boolean (Effect Unit) Unit
foreign import forE :: forall a . EFn.EffectFn3 Int Int (EFn.EffectFn1 Int a) Unit
foreign import foreachE :: forall a b. EFn.EffectFn2 (Array a) (EFn.EffectFn1 a b) Unit
foreign import sequenceE :: forall a. EFn.EffectFn1 (Array (Effect a)) Unit
foreign import tailRecE :: forall r. EFn.EffectFn2 (EFn.EffectFn1 (Step r) (Step r)) (Step r) (Step r)
