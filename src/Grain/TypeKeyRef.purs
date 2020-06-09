module Grain.TypeKeyRef
  ( TypeKeyRef
  , new
  , read
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import Grain.TypeRef (TypeRef)

foreign import data TypeKeyRef :: Type

read :: TypeRef -> TypeKeyRef -> Effect String
read typeRef typeKeyRef = do
  maybeTypeKey <- get_ typeRef typeKeyRef
  case maybeTypeKey of
    Just typeKey -> pure typeKey
    Nothing -> do
      typeKey <- randomKey
      set_ typeRef typeKey typeKeyRef
      pure typeKey

get_ :: TypeRef -> TypeKeyRef -> Effect (Maybe String)
get_ typeRef typeKeyRef =
  toMaybe <$> runEffectFn2 getImpl typeRef typeKeyRef

set_ :: TypeRef -> String -> TypeKeyRef -> Effect Unit
set_ typeRef typeKey typeKeyRef =
  runEffectFn3 setImpl typeRef typeKey typeKeyRef

foreign import new :: Effect TypeKeyRef

foreign import getImpl :: EffectFn2 TypeRef TypeKeyRef (Nullable String)

foreign import setImpl :: EffectFn3 TypeRef String TypeKeyRef Unit

foreign import randomKey :: Effect String
