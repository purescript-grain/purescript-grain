module Main where

import Prelude

import Data.DateTime (DateTime)
import Data.JSDate (fromDateTime, toString)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Now (nowDateTime)
import Effect.Timer (setInterval)
import Grain (class GlobalGrain, GProxy(..), VNode, fromConstructor, mount, useUpdater, useValue)
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
      mount view $ toNode el

newtype Now = Now DateTime

instance globalGrainNow :: GlobalGrain Now where
  initialState _ = Now <$> nowDateTime
  typeRefOf _ = fromConstructor Now

view :: VNode
view = H.component do
  updateNow <- useUpdater (GProxy :: _ Now)

  let listenNow = void $ setInterval 5000 do
        now <- nowDateTime
        updateNow $ const $ Now now

  pure $ H.div # H.didCreate (const listenNow) # H.kids
    [ nowView
    ]

nowView :: VNode
nowView = H.component do
  Now now <- useValue (GProxy :: _ Now)

  pure $ H.span # H.kids
    [ H.text $ toString $ fromDateTime now
    ]
