module Grain.Internal.MObject
  ( MObject
  , new
  , keys
  , values
  , has
  , get
  , set
  , del
  , unsafeGet
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried as EFn

foreign import data MObject :: Type -> Type

foreign import new :: forall a. Effect (MObject a)

foreign import keys :: forall a. EFn.EffectFn1 (MObject a) (Array String)
foreign import values :: forall a. EFn.EffectFn1 (MObject a) (Array a)
foreign import has :: forall a. EFn.EffectFn2 String (MObject a) Boolean

get ::  forall a. EFn.EffectFn2 String (MObject a) (Maybe a)
get = EFn.mkEffectFn2 \k o -> do
  nullable <- EFn.runEffectFn2 getImpl k o
  pure $ toMaybe nullable

foreign import getImpl :: forall a. EFn.EffectFn2 String (MObject a) (Nullable a)
foreign import set :: forall a. EFn.EffectFn3 String a (MObject a) Unit
foreign import del :: forall a. EFn.EffectFn2 String (MObject a) Unit
foreign import unsafeGet :: forall a. EFn.EffectFn2 String (MObject a) a
