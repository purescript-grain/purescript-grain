module Grain.UI.Handler
  ( allocHandlers
  , updateHandlers
  ) where

import Prelude

import Data.Array (union)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Tuple (Tuple(..), fst, lookup)
import Effect (Effect)
import Grain.Effect (foreachE)
import Grain.UI.Util (setAny)
import Web.DOM.Element (Element)
import Web.Event.Event (Event)
import Web.Event.EventTarget (eventListener)

type AllocArgs =
  { nexts :: Array (Tuple String (Event -> Effect Unit))
  , element :: Element
  }

allocHandlers :: AllocArgs -> Effect Unit
allocHandlers args =
  foreachE args.nexts \(Tuple name handler) ->
    setHandler { name, handler, element: args.element }

type UpdateArgs =
  { currents :: Array (Tuple String (Event -> Effect Unit))
  , nexts :: Array (Tuple String (Event -> Effect Unit))
  , element :: Element
  }

updateHandlers :: UpdateArgs -> Effect Unit
updateHandlers args =
  let names = union (fst <$> args.currents) (fst <$> args.nexts)
      updateByName name =
        case lookup name args.currents, lookup name args.nexts of
          Nothing, Nothing ->
            pure unit
          Just _, Nothing ->
            removeHandler { name, element: args.element }
          _, Just handler ->
            setHandler { name, handler, element: args.element }
   in foreachE names updateByName

type SetArgs =
  { name :: String
  , handler :: Event -> Effect Unit
  , element :: Element
  }

setHandler :: SetArgs -> Effect Unit
setHandler args = do
  listener <- eventListener args.handler
  setAny args.name listener args.element

type RemoveArgs =
  { name :: String
  , element :: Element
  }

removeHandler :: RemoveArgs -> Effect Unit
removeHandler args = setAny args.name null args.element
