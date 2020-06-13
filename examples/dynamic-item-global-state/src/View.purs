module View
  ( view
  ) where

import Prelude

import Data.Array (last, snoc)
import Data.Maybe (fromMaybe)
import Data.Newtype (over)
import Grain (VNode, grain, useGlobalUpdater, useGlobalValue)
import Grain.Markup as H
import State.ItemIds (ItemIds(..))
import View.Item as Item

view :: VNode
view = H.component do
  ItemIds itemIds <- useGlobalValue (grain :: _ ItemIds)
  updateItemIds <- useGlobalUpdater (grain :: _ ItemIds)

  let addItem = updateItemIds $ over ItemIds \ids -> snoc ids $ (fromMaybe 0 $ last ids) + 1

  pure $ H.div # H.kids
    [ H.button # H.onClick (const addItem) # H.kids [ H.text "ADD" ]
    , H.ul # H.kids (Item.view <$> itemIds)
    ]
