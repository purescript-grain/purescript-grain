module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Effect (Effect)
import Grain (class LocalGrain, LProxy(..), VNode, fromConstructor, mountUI, useUpdater, useValue)
import Grain.Markup as H
import Web.DOM.Element (toNode)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  maybeEl <- window >>= document <#> toParentNode >>= querySelector (QuerySelector "#app")
  case maybeEl of
    Nothing -> pure unit
    Just el ->
      void $ mountUI view $ toNode el

newtype Count = Count Int

derive newtype instance showCount :: Show Count

derive instance newtypeCount :: Newtype Count _

instance localGrainCount :: LocalGrain Count where
  initialState _ = pure $ Count 0
  typeRefOf _ = fromConstructor Count

view :: VNode
view = H.component do
  count <- useValue (LProxy :: _ Count)
  updateCount <- useUpdater (LProxy :: _ Count)
  let increment = updateCount $ over Count (_ + 1)
      decrement = updateCount $ over Count (_ - 1)
  pure $ H.div # H.css containerStyles # H.kids
    [ H.button
        # H.css buttonStyles
        # H.onClick (const decrement)
        # H.kids [ H.text "-" ]
    , H.div
        # H.css countStyles
        # H.kids [ H.text $ show count ]
    , H.button
        # H.css buttonStyles
        # H.onClick (const increment)
        # H.kids [ H.text "+" ]
    ]
  where
    containerStyles = """
    body {
      margin: 0;
    }
    .& {
      height: 100vh;
      display: flex;
      justify-content: center;
      align-items: center;
    }
    """
    buttonStyles = """
    .& {
      width: 32px;
      height: 32px;
      display: flex;
      justify-content: center;
      align-items: center;
      font-size: 24px;
      cursor: pointer;
    }
    """
    countStyles = """
    .& {
      font-size: 48px;
      font-weight: bold;
      margin: 0 24px;
    }
    """
