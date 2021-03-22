module Grain.Class.LProxy
  ( LProxy(..)
  , class LocalGrain
  , initialState
  , typeRefOf
  ) where

import Effect (Effect)
import Grain.TypeRef (TypeRef)

-- | A type of component-local state proxy.
data LProxy :: Type -> Type
data LProxy a = LProxy

-- | Representation of a component-local state.
-- |
-- | `TypeRef` is used as state key internally for uniqueness.
class LocalGrain a where
  initialState :: LProxy a -> Effect a
  typeRefOf :: LProxy a -> TypeRef
