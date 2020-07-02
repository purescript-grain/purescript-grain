module Grain.MMap
  ( MMap
  , new
  , get
  , set
  , del
  , unsafeGet
  ) where

import Prelude

import Data.Maybe (Maybe, fromJust)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Partial.Unsafe (unsafePartial)

foreign import data MMap :: Type -> Type -> Type

foreign import new :: forall a b. Effect (MMap a b)

get :: forall a b. a -> MMap a b -> Effect (Maybe b)
get k m = toMaybe <$> runEffectFn2 getImpl k m

set :: forall a b. a -> b -> MMap a b -> Effect Unit
set k v m = runEffectFn3 setImpl k v m

del :: forall a b. a -> MMap a b -> Effect Unit
del k m = runEffectFn2 delImpl k m

unsafeGet :: forall a b. a -> MMap a b -> Effect b
unsafeGet k m = unsafePartial $ fromJust <$> get k m

foreign import getImpl :: forall a b. EffectFn2 a (MMap a b) (Nullable b)
foreign import setImpl :: forall a b. EffectFn3 a b (MMap a b) Unit
foreign import delImpl :: forall a b. EffectFn2 a (MMap a b) Unit
