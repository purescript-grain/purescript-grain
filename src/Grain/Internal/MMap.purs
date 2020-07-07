module Grain.Internal.MMap
  ( MMap
  , new
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

foreign import data MMap :: Type -> Type -> Type

foreign import new :: forall a b. Effect (MMap a b)

get :: forall a b. EFn.EffectFn2 a (MMap a b) (Maybe b)
get = EFn.mkEffectFn2 \k m -> do
  nullable <- EFn.runEffectFn2 getImpl k m
  pure $ toMaybe nullable

foreign import getImpl :: forall a b. EFn.EffectFn2 a (MMap a b) (Nullable b)
foreign import set :: forall a b. EFn.EffectFn3 a b (MMap a b) Unit
foreign import del :: forall a b. EFn.EffectFn2 a (MMap a b) Unit
foreign import unsafeGet :: forall a b. EFn.EffectFn2 a (MMap a b) b
