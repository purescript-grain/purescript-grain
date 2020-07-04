module Grain.Internal.Emitter
  ( Emitter
  , createEmitter
  , subscribe
  , unsubscribe
  , emit
  ) where

import Prelude

import Data.Array (filter, snoc)
import Effect (Effect)
import Effect.Ref (Ref, modify_, new, read)
import Effect.Uncurried as EFn
import Grain.Internal.Effect (sequenceE)
import Unsafe.Reference (unsafeRefEq)

newtype Emitter = Emitter (Ref (Array (Effect Unit)))

createEmitter :: Effect Emitter
createEmitter = do
  listenersRef <- new []
  pure $ Emitter listenersRef

subscribe :: EFn.EffectFn2 (Effect Unit) Emitter Unit
subscribe = EFn.mkEffectFn2 \listener (Emitter listenersRef) ->
  modify_ (_ `snoc` listener) listenersRef

unsubscribe :: EFn.EffectFn2 (Effect Unit) Emitter Unit
unsubscribe = EFn.mkEffectFn2 \listener (Emitter listenersRef) ->
  modify_ (filter (not <<< unsafeRefEq listener)) listenersRef

emit :: EFn.EffectFn1 Emitter Unit
emit = EFn.mkEffectFn1 \(Emitter listenersRef) -> do
  listeners <- read listenersRef
  EFn.runEffectFn1 sequenceE listeners
