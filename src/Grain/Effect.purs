module Grain.Effect
  ( forE
  , foreachE
  , sequenceE
  , forObjectE
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Foreign.Object (Object)

forE :: forall a. Int -> Int -> (Int -> Effect a) -> Effect Unit
forE lo hi f = runEffectFn3 forEImpl lo hi f

foreachE :: forall a b. Array a -> (a -> Effect b) -> Effect Unit
foreachE xs f = runEffectFn2 foreachEImpl xs f

sequenceE :: forall a. (Array (Effect a)) -> Effect Unit
sequenceE fs = runEffectFn1 sequenceEImpl fs

forObjectE :: forall a b. Object a -> (String -> a -> Effect b) -> Effect Unit
forObjectE obj f = runEffectFn2 forObjectEImpl obj f

foreign import forEImpl :: forall a. EffectFn3 Int Int (Int -> Effect a) Unit
foreign import foreachEImpl :: forall a b. EffectFn2 (Array a) (a -> Effect b) Unit
foreign import sequenceEImpl :: forall a. EffectFn1 (Array (Effect a)) Unit
foreign import forObjectEImpl :: forall a b. EffectFn2 (Object a) (String -> a -> Effect b) Unit
