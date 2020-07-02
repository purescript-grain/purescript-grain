module Grain.Effect
  ( forE
  , foreachE
  , sequenceE
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)

forE :: forall a. Int -> Int -> (Int -> Effect a) -> Effect Unit
forE lo hi f = runEffectFn3 forEImpl lo hi f

foreachE :: forall a b. Array a -> (a -> Effect b) -> Effect Unit
foreachE xs f = runEffectFn2 foreachEImpl xs f

sequenceE :: forall a. (Array (Effect a)) -> Effect Unit
sequenceE fs = runEffectFn1 sequenceEImpl fs

foreign import forEImpl :: forall a. EffectFn3 Int Int (Int -> Effect a) Unit
foreign import foreachEImpl :: forall a b. EffectFn2 (Array a) (a -> Effect b) Unit
foreign import sequenceEImpl :: forall a. EffectFn1 (Array (Effect a)) Unit
