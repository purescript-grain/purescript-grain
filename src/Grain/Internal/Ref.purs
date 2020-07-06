module Grain.Internal.Ref where

import Prelude

import Effect.Uncurried as EFn

foreign import data Ref :: Type -> Type

modify_ :: forall a. EFn.EffectFn2 (a -> a) (Ref a) Unit
modify_ = EFn.mkEffectFn2 \f ref ->
  void $ EFn.runEffectFn2 modify f ref

foreign import new :: forall a. EFn.EffectFn1 a (Ref a)
foreign import read :: forall a. EFn.EffectFn1 (Ref a) a
foreign import modify :: forall a. EFn.EffectFn2 (a -> a) (Ref a) a
foreign import write :: forall a. EFn.EffectFn2 a (Ref a) Unit
