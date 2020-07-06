module Grain.Internal.Util
  ( nonNull
  , byIdx
  , byIdxNullable
  , mapNullable
  , eqNullable
  , raf
  , head
  , nodeIndexOf
  , createTextNode
  , createElement
  , createElementNS
  , appendChild
  , removeChild
  , putChild
  , setTextContent
  , setAny
  , setAttribute
  , removeAttribute
  , isProperty
  , isBoolean
  , mkEventListener
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

nonNull :: forall a. Nullable a -> a
nonNull = unsafeCoerce

foreign import byIdx :: forall a. Fn.Fn2 (Array a) Int a
foreign import byIdxNullable :: forall a. Fn.Fn2 (Array a) Int (Nullable a)
foreign import mapNullable :: forall a b. Fn.Fn2 (a -> b) (Nullable a) (Nullable b)
foreign import eqNullable :: forall a. Fn.Fn2 (Nullable a) (Nullable a) Boolean
foreign import raf :: EFn.EffectFn1 (Effect Unit) Unit
foreign import head :: Effect Node
foreign import createTextNode :: EFn.EffectFn1 String Text
foreign import createElement :: EFn.EffectFn1 String Element
foreign import createElementNS :: EFn.EffectFn1 String Element
foreign import appendChild :: EFn.EffectFn2 Node Node Node
foreign import removeChild :: EFn.EffectFn2 Node Node Unit
foreign import putChild :: EFn.EffectFn3 Int Node Node Unit
foreign import nodeIndexOf :: EFn.EffectFn1 Node Int
foreign import setTextContent :: EFn.EffectFn2 String Node Unit
foreign import setAny :: forall a. EFn.EffectFn3 String a Element Unit
foreign import setAttribute :: EFn.EffectFn3 String String Element Unit
foreign import removeAttribute :: EFn.EffectFn2 String Element Unit
foreign import isProperty :: Fn.Fn2 String Element Boolean
foreign import isBoolean :: Fn.Fn2 String Element Boolean
foreign import mkEventListener :: (Event -> Effect Unit) -> EventListener
