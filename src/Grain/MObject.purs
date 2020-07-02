module Grain.MObject
  ( MObject
  , new
  , keys
  , values
  , size
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
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Unsafe.Coerce (unsafeCoerce)

foreign import data MObject :: Type -> Type

foreign import new :: forall a. Effect (MObject a)

keys :: forall a. MObject a -> Effect (Array String)
keys o = runEffectFn1 keysImpl o

values :: forall a. MObject a -> Effect (Array a)
values o = runEffectFn1 valuesImpl o

size :: forall a. MObject a -> Effect Int
size o = runEffectFn1 sizeImpl o

has :: forall a. String -> MObject a -> Effect Boolean
has k o = runEffectFn2 hasImpl k o

get :: forall a. String -> MObject a -> Effect (Maybe a)
get k o = toMaybe <$> runEffectFn2 getImpl k o

set :: forall a. String -> a -> MObject a -> Effect Unit
set k v o = runEffectFn3 setImpl k v o

del :: forall a. String -> MObject a -> Effect Unit
del k o = runEffectFn2 delImpl k o

unsafeGet :: forall a. String -> MObject a -> Effect a
unsafeGet k o = unsafeCoerce $ runEffectFn2 getImpl k o

foreign import keysImpl :: forall a. EffectFn1 (MObject a) (Array String)
foreign import valuesImpl :: forall a. EffectFn1 (MObject a) (Array a)
foreign import sizeImpl :: forall a. EffectFn1 (MObject a) Int
foreign import hasImpl :: forall a. EffectFn2 String (MObject a) Boolean
foreign import getImpl :: forall a. EffectFn2 String (MObject a) (Nullable a)
foreign import setImpl :: forall a. EffectFn3 String a (MObject a) Unit
foreign import delImpl :: forall a. EffectFn2 String (MObject a) Unit
