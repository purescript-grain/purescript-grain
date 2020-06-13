module State.ItemIds where

import Prelude

import Data.Newtype (class Newtype)
import Grain (class Grain, fromConstructor)

newtype ItemIds = ItemIds (Array Int)

derive instance newtypeItemIds :: Newtype ItemIds _

instance grainItemIds :: Grain ItemIds where
  initialState _ = pure $ ItemIds []
  typeRefOf _ = fromConstructor ItemIds
