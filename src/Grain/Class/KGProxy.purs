module Grain.Class.KGProxy
  ( KGProxy(..)
  , class GrainKey
  , stringifyKey
  , class KeyedGlobalGrain
  , initialState
  , typeRefOf
  ) where

import Prelude

import Effect (Effect)
import Grain.TypeRef (TypeRef)

-- | A type of keyed global state proxy.
data KGProxy :: Type -> Type -> Type
data KGProxy k a = KGProxy

-- | Representation of key type for keyed global state.
-- |
-- | This class's instances can be key for keyed global state.
class GrainKey a where
  stringifyKey :: a -> String

instance grainKeyString :: GrainKey String where
  stringifyKey = identity

-- | Representation of a partial state of application state.
-- |
-- | You can use this to define global state with key for dynamic items.
-- |
-- | The first parameter is a type of key for each item, and the second parameter is a type of item.
-- |
-- | `TypeRef` is used as state key internally for uniqueness.
class (GrainKey k) <= KeyedGlobalGrain k a | a -> k where
  initialState :: KGProxy k a -> Effect a
  typeRefOf :: KGProxy k a -> TypeRef
