module Grain.Internal.Handler
  ( Handlers
  , allocHandlers
  , updateHandlers
  ) where

import Prelude

import Data.Nullable (null)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried as EFn
import Grain.Internal.PropDiff (Create, Delete, Update, Patch, diff)
import Grain.Internal.Util (foreachE, mkEventListener, setAny)
import Web.DOM.Element (Element)
import Web.Event.Event (Event)

type Handlers =
  Array (Tuple String (Event -> Effect Unit))

allocHandlers :: EFn.EffectFn2 Handlers Element Unit
allocHandlers = EFn.mkEffectFn2 \nexts element ->
  EFn.runEffectFn2 foreachE nexts $ EFn.mkEffectFn1 \(Tuple name handler) ->
    EFn.runEffectFn3 setHandler name handler element

updateHandlers :: EFn.EffectFn3 Handlers Handlers Element Unit
updateHandlers = EFn.mkEffectFn3 \currents nexts element ->
  EFn.runEffectFn2 diff patch { context: element, currents, nexts }

patch :: Patch Element (Event -> Effect Unit)
patch = { create, delete, update }

create :: Create Element (Event -> Effect Unit)
create =
  EFn.mkEffectFn2 \element (Tuple name handler) ->
    EFn.runEffectFn3 setHandler name handler element

delete :: Delete Element (Event -> Effect Unit)
delete =
  EFn.mkEffectFn2 \element (Tuple name _) ->
    EFn.runEffectFn3 setAny name null element

update :: Update Element (Event -> Effect Unit)
update =
  EFn.mkEffectFn3 \element _ (Tuple name handler) ->
    EFn.runEffectFn3 setHandler name handler element

setHandler :: EFn.EffectFn3 String (Event -> Effect Unit) Element Unit
setHandler = EFn.mkEffectFn3 \name handler element ->
  let listener = mkEventListener handler
   in EFn.runEffectFn3 setAny name listener element
