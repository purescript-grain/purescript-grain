module Grain.Class
  ( GProxy(..)
  , grain
  , grainWithKey
  , class Grain
  , initialState
  , typeRefOf
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Grain.TypeRef (TypeRef)

data GProxy a = GProxy (Maybe String)

grain :: forall a. GProxy a
grain = GProxy Nothing

grainWithKey :: forall a. String -> GProxy a
grainWithKey = GProxy <<< Just

-- | Representation of a partial state of application state.
class Grain a where
  initialState :: GProxy a -> Effect a
  typeRefOf :: GProxy a -> TypeRef
