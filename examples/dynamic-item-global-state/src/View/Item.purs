module View.Item
  ( view
  ) where

import Prelude

import Data.Array (delete)
import Data.Newtype (over)
import Grain (GProxy(..), KGProxy(..), VNode, useKeyedUpdater, useKeyedValue, useUpdater)
import Grain.Markup as H
import State.Item (Item(..))
import State.ItemIds (ItemIds(..))

view :: Int -> VNode
view id =
  H.key (show id) $ H.fingerprint (show id) $ H.component do
    -- Store each item states globally.
    Item item <- useKeyedValue KGProxy $ show id
    updateItem <- useKeyedUpdater (KGProxy :: _ Item)
    updateItemIds <- useUpdater (GProxy :: _ ItemIds)

    let toggleClicked =
          updateItem (show id) \(Item i) ->
            Item i { clicked = not i.clicked }

        deleteItem =
          updateItemIds
            $ over ItemIds $ delete id

    pure $ H.li # H.kids
      [ H.button # H.onClick (const deleteItem) # H.kids [ H.text "DELETE" ]
      , H.span # H.onClick (const toggleClicked) # H.kids
          [ H.text $ item.name <> show id <> if item.clicked then " clicked!" else ""
          ]
      ]
