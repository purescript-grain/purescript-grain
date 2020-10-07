module Grain.Internal.Store
  ( Store
  , createStore
  , readGrain
  , subscribeGrain
  , unsubscribeGrain
  , updateGrain
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried as EFn
import Foreign (Foreign, unsafeFromForeign, unsafeToForeign)
import Grain.Class (class Grain, initialState, typeRefOf)
import Grain.Internal.Emitter (Emitter, createEmitter, emit, subscribe, unsubscribe)
import Grain.Internal.MMap (MMap)
import Grain.Internal.MMap as MM
import Grain.Internal.MObject (MObject)
import Grain.Internal.MObject as MO
import Grain.Internal.Ref (Ref)
import Grain.Internal.Ref as Ref
import Grain.TypeRef (TypeRef)
import Unsafe.Coerce (unsafeCoerce)

newtype Store = Store (MMap TypeRef (MObject Part))

type Part =
  { emitter :: Emitter
  , valueRef :: Ref Foreign
  }

createStore :: Effect Store
createStore = do
  m <- MM.new
  pure $ Store m

readGrain
  :: forall p a
   . Grain p a
  => EFn.EffectFn3 (p a) String Store a
readGrain = EFn.mkEffectFn3 \proxy k store -> do
  { valueRef } <- EFn.runEffectFn3 lookupPart proxy k store
  value <- EFn.runEffectFn1 Ref.read valueRef
  pure $ unsafeFromForeign value

subscribeGrain
  :: forall p a
   . Grain p a
  => EFn.EffectFn4 (p a) String (Effect Unit) Store Unit
subscribeGrain = EFn.mkEffectFn4 \proxy k listener store -> do
  { emitter } <- EFn.runEffectFn3 lookupPart proxy k store
  EFn.runEffectFn2 subscribe listener emitter

unsubscribeGrain
  :: forall p a
   . Grain p a
  => EFn.EffectFn4 (p a) String (Effect Unit) Store Unit
unsubscribeGrain = EFn.mkEffectFn4 \proxy k listener store -> do
  { emitter } <- EFn.runEffectFn3 lookupPart proxy k store
  EFn.runEffectFn2 unsubscribe listener emitter

updateGrain
  :: forall p a
   . Grain p a
  => EFn.EffectFn4 (p a) String (a -> a) Store Unit
updateGrain = EFn.mkEffectFn4 \proxy k f store -> do
  { emitter, valueRef } <- EFn.runEffectFn3 lookupPart proxy k store
  EFn.runEffectFn2 Ref.modify_ (unsafeCoerce f) valueRef
  EFn.runEffectFn1 emit emitter

lookupPart
  :: forall p a
   . Grain p a
  => EFn.EffectFn3 (p a) String Store Part
lookupPart = EFn.mkEffectFn3 \proxy k store -> do
  partsRef <- EFn.runEffectFn2 lookupPartsRef proxy store
  mPart <- EFn.runEffectFn2 MO.get k partsRef
  case mPart of
    Just part -> pure part
    Nothing -> do
      value <- initialState proxy
      valueRef <- EFn.runEffectFn1 Ref.new $ unsafeToForeign value
      emitter <- createEmitter
      let part = { emitter, valueRef }
      EFn.runEffectFn3 MO.set k part partsRef
      pure part

lookupPartsRef
  :: forall p a
   . Grain p a
  => EFn.EffectFn2 (p a) Store (MObject Part)
lookupPartsRef = EFn.mkEffectFn2 \proxy (Store m) -> do
  maybePartsRef <- EFn.runEffectFn2 MM.get (typeRefOf proxy) m
  case maybePartsRef of
    Just partsRef ->
      pure partsRef
    Nothing -> do
      partsRef <- MO.new
      EFn.runEffectFn3 MM.set (typeRefOf proxy) partsRef m
      pure partsRef
