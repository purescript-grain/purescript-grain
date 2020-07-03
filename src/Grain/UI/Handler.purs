module Grain.UI.Handler
  ( allocHandlers
  , updateHandlers
  ) where

import Prelude

import Data.Nullable (null)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Grain.Effect (foreachE)
import Grain.UI.PropDiff (PatchArgs(..), diff)
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
  diff (patch args.element)
    { currents: args.currents
    , nexts: args.nexts
    }

patch
  :: Element
  -> PatchArgs (Event -> Effect Unit)
  -> Effect Unit
patch element (Create { next: Tuple name handler }) =
  setHandler { name, handler, element }
patch element (Update { next: Tuple name handler }) =
  setHandler { name, handler, element }
patch element (Delete { current: Tuple name _ }) =
  removeHandler { name, element }

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
