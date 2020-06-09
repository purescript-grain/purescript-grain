module View
  ( view
  ) where

import Prelude

import Data.Array (last, snoc)
import Data.Maybe (fromMaybe)
import Grain (VNode, useGlobalUpdater, useGlobalValue)
import Grain.Markup as H
import State.ItemIds (ItemIds(..))
import View.Item as Item

view :: VNode
view = H.component do
  itemIds <- useGlobalValue ItemIds
  updateItemIds <- useGlobalUpdater ItemIds

  let addItem = updateItemIds \ids -> snoc ids $ (fromMaybe 0 $ last ids) + 1

  pure $ H.div # H.kids
    [ H.button # H.onClick (const addItem) # H.kids [ H.text "ADD" ]
    , H.ul # H.kids (Item.view <$> itemIds)
    ]
