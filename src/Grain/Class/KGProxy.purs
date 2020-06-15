module Grain.Class.KGProxy
  ( KGProxy(..)
  , class KeyedGlobalGrain
  , initialState
  , typeRefOf
  ) where

import Effect (Effect)
import Grain.TypeRef (TypeRef)

-- | A type of keyed global state proxy.
data KGProxy a = KGProxy String

-- | Representation of a partial state of application state.
-- |
-- | You can use this to define global state with key for dynamic items.
-- |
-- | `TypeRef` is used as state key internally for uniqueness.
class KeyedGlobalGrain a where
  initialState :: KGProxy a -> Effect a
  typeRefOf :: KGProxy a -> TypeRef
