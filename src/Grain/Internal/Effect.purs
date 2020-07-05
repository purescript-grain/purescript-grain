module Grain.Internal.Effect
  ( Step
  , whenE
  , forE
  , foreachE
  , sequenceE
  , tailRecE
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried as EFn

type Step r = { done :: Boolean | r }

foreign import whenE :: EFn.EffectFn2 Boolean (Effect Unit) Unit
foreign import forE :: forall a . EFn.EffectFn3 Int Int (EFn.EffectFn1 Int a) Unit
foreign import foreachE :: forall a b. EFn.EffectFn2 (Array a) (EFn.EffectFn1 a b) Unit
foreign import sequenceE :: forall a. EFn.EffectFn1 (Array (Effect a)) Unit
foreign import tailRecE :: forall r. EFn.EffectFn2 (EFn.EffectFn1 (Step r) (Step r)) (Step r) (Step r)
