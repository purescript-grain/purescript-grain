module View
  ( view
  ) where

import Prelude

import Data.Array (last, snoc)
import Data.Maybe (fromMaybe)
import Data.Newtype (over)
import Grain (GProxy(..), VNode, useUpdater, useValue)
import Grain.Markup as H
import State.ItemIds (ItemIds(..))
import View.Item as Item

view :: VNode
view = H.component do
  ItemIds itemIds <- useValue (GProxy :: _ ItemIds)
  updateItemIds <- useUpdater (GProxy :: _ ItemIds)

  let addItem = updateItemIds $ over ItemIds \ids -> snoc ids $ (fromMaybe 0 $ last ids) + 1

  pure $ H.div # H.kids
    [ H.button # H.onClick (const addItem) # H.kids [ H.text "ADD" ]
    , H.ul # H.kids (Item.view <$> itemIds)
    ]
