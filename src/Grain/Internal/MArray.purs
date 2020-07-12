module Grain.Internal.MArray where

import Prelude

import Effect (Effect)
import Effect.Uncurried as EFn
import Unsafe.Coerce (unsafeCoerce)

foreign import data MArray :: Type -> Type

toArray :: forall a. MArray a -> Array a
toArray = unsafeCoerce

foreign import new :: forall a. Effect (MArray a)
foreign import cons :: forall a. EFn.EffectFn2 a (MArray a) Unit
foreign import snoc :: forall a. EFn.EffectFn2 (MArray a) a Unit
foreign import cutFrom :: forall a. EFn.EffectFn2 Int (MArray a) Unit
foreign import deleteIfEqRef :: forall a. EFn.EffectFn2 a (MArray a) Unit
foreign import clear :: forall a. EFn.EffectFn1 (MArray a) Unit
