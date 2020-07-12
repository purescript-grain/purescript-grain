module Grain.Internal.Emitter
  ( Emitter
  , createEmitter
  , subscribe
  , unsubscribe
  , emit
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried as EFn
import Grain.Internal.MArray (MArray)
import Grain.Internal.MArray as MA
import Grain.Internal.Util (sequenceE)

newtype Emitter = Emitter (MArray (Effect Unit))

createEmitter :: Effect Emitter
createEmitter = do
  listenersRef <- MA.new
  pure $ Emitter listenersRef

subscribe :: EFn.EffectFn2 (Effect Unit) Emitter Unit
subscribe = EFn.mkEffectFn2 \listener (Emitter listenersRef) ->
  EFn.runEffectFn2 MA.snoc listenersRef listener

unsubscribe :: EFn.EffectFn2 (Effect Unit) Emitter Unit
unsubscribe = EFn.mkEffectFn2 \listener (Emitter listenersRef) ->
  EFn.runEffectFn2 MA.deleteIfEqRef listener listenersRef

emit :: EFn.EffectFn1 Emitter Unit
emit = EFn.mkEffectFn1 \(Emitter listenersRef) ->
  EFn.runEffectFn1 sequenceE $ MA.toArray listenersRef
