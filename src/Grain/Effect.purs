module Grain.Effect
  ( sequenceE
  , traverseE
  , forObjectE
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Foreign.Object (Object)

sequenceE :: forall a. (Array (Effect a)) -> Effect Unit
sequenceE fs = runEffectFn1 sequenceEImpl fs

traverseE :: forall a b. (a -> Effect b) -> Array a -> Effect (Array b)
traverseE f xs = runEffectFn2 traverseEImpl f xs

forObjectE :: forall a b. Object a -> (String -> a -> Effect b) -> Effect Unit
forObjectE obj f = runEffectFn2 forObjectEImpl obj f

foreign import sequenceEImpl :: forall a. EffectFn1 (Array (Effect a)) Unit
foreign import traverseEImpl :: forall a b. EffectFn2 (a -> Effect b) (Array a) (Array b)
foreign import forObjectEImpl :: forall a b. EffectFn2 (Object a) (String -> a -> Effect b) Unit
