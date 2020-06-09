module View.Item
  ( view
  ) where

import Prelude

import Data.Array (delete)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Grain (class Grain, VNode, fromConstructor, useGlobalUpdater, useGlobalValue)
import Grain.Markup as H
import State.ItemIds (ItemIds(..))

newtype Item = Item
  { id :: Int
  , name :: String
  , clicked :: Boolean
  }

newtype ItemKey = ItemKey Int

derive instance genericItemKey :: Generic ItemKey _

instance showItemKey :: Show ItemKey where
  show = genericShow

instance grainItemKey :: Grain ItemKey Item where
  initialState (ItemKey id) =
    pure $ Item
      { id
      , name: "Globally stored Item " <> show id
      , clicked: false
      }
  typeRefOf _ = fromConstructor ItemKey

view :: Int -> VNode
view id =
  H.key (show id) $ H.fingerprint (show id) $ H.component do
    -- Store each item states globally.
    Item item <- useGlobalValue $ ItemKey id
    updateItem <- useGlobalUpdater $ ItemKey id
    updateItemIds <- useGlobalUpdater ItemIds

    let toggleClicked =
          updateItem \(Item i) ->
            Item i { clicked = not i.clicked }

        deleteItem =
          updateItemIds $ delete item.id

    pure $ H.li # H.kids
      [ H.button # H.onClick (const deleteItem) # H.kids [ H.text "DELETE" ]
      , H.span # H.onClick (const toggleClicked) # H.kids
          [ H.text $ item.name <> if item.clicked then " clicked!" else ""
          ]
      ]
