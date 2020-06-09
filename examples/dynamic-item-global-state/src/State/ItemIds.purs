module State.ItemIds where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Grain (class Grain, fromConstructor)

data ItemIds = ItemIds

derive instance genericItemIds :: Generic ItemIds _

instance showItemsIds :: Show ItemIds where
  show = genericShow

instance grainItemIds :: Grain ItemIds (Array Int) where
  initialState _ = pure []
  typeRefOf _ = fromConstructor ItemIds
