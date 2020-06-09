module Grain.UI.Handler
  ( allocHandlers
  , updateHandlers
  ) where

import Prelude

import Control.Safely (for_)
import Data.Array (union)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Tuple (Tuple, uncurry)
import Effect (Effect)
import Foreign (unsafeToForeign)
import Foreign.Object (Object, keys, lookup, toUnfoldable)
import Grain.UI.Util (setForeign)
import Web.DOM.Element (Element)
import Web.Event.Event (Event)
import Web.Event.EventTarget (eventListener)

allocHandlers
  :: Object (Event -> Effect Unit)
  -> Element
  -> Effect Unit
allocHandlers handlers el =
  let handlers' :: Array (Tuple String (Event -> Effect Unit))
      handlers' = toUnfoldable handlers
   in for_ handlers' $ uncurry \name handler -> setHandler name handler el

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
   in for_ names updateByName

setHandler
  :: String
  -> (Event -> Effect Unit)
  -> Element
  -> Effect Unit
setHandler name handler el = do
  listener <- eventListener handler
  setForeign name (unsafeToForeign listener) el

removeHandler :: String -> Element -> Effect Unit
removeHandler name = setForeign name (unsafeToForeign null)
