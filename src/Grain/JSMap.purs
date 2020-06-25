module Grain.JSMap
  ( JSMap
  , new
  , get
  , set
  , del
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)

foreign import data JSMap :: Type -> Type -> Type

foreign import new :: forall a b. Effect (JSMap a b)

get :: forall a b. a -> JSMap a b -> Effect (Maybe b)
get k m = toMaybe <$> runEffectFn2 getImpl k m

set :: forall a b. a -> b -> JSMap a b -> Effect Unit
set k v m = runEffectFn3 setImpl k v m

del :: forall a b. a -> JSMap a b -> Effect Unit
del k m = runEffectFn2 delImpl k m

foreign import getImpl :: forall a b. EffectFn2 a (JSMap a b) (Nullable b)
foreign import setImpl :: forall a b. EffectFn3 a b (JSMap a b) Unit
foreign import delImpl :: forall a b. EffectFn2 a (JSMap a b) Unit
