module Grain.UI.Handler
  ( allocHandlers
  , updateHandlers
  ) where

import Prelude

import Data.Array (union)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Effect (Effect, foreachE)
import Foreign.Object (Object, keys, lookup)
import Grain.Effect (forObjectE)
import Grain.UI.Util (setAny)
import Web.DOM.Element (Element)
import Web.Event.Event (Event)
import Web.Event.EventTarget (eventListener)

allocHandlers
  :: Object (Event -> Effect Unit)
  -> Element
  -> Effect Unit
allocHandlers handlers el =
  forObjectE handlers \name handler ->
    setHandler name handler el

updateHandlers
  :: Object (Event -> Effect Unit)
  -> Object (Event -> Effect Unit)
  -> Element
  -> Effect Unit
updateHandlers currents nexts el =
  let names = union (keys currents) (keys nexts)
      updateByName name =
        case lookup name currents, lookup name nexts of
          Nothing, Nothing -> pure unit
          Just _, Nothing -> removeHandler name el
          _, Just handler -> setHandler name handler el
   in foreachE names updateByName

setHandler
  :: String
  -> (Event -> Effect Unit)
  -> Element
  -> Effect Unit
setHandler name handler el = do
  listener <- eventListener handler
  setAny name listener el

removeHandler :: String -> Element -> Effect Unit
removeHandler name = setAny name null
