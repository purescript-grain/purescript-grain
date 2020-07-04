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
import Grain.Internal.Effect (foreachE)
import Grain.Internal.PropDiff (PatchArgs(..), diff)
import Grain.Internal.Util (mkEventListener, setAny)
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
  EFn.runEffectFn2 diff (patch element) { currents, nexts }

patch
  :: Element
  -> EFn.EffectFn1 (PatchArgs (Event -> Effect Unit)) Unit
patch element = EFn.mkEffectFn1 \act ->
  case act of
    Create { next: Tuple name handler } ->
      EFn.runEffectFn3 setHandler name handler element
    Update { next: Tuple name handler } ->
      EFn.runEffectFn3 setHandler name handler element
    Delete { current: Tuple name _ } ->
      EFn.runEffectFn3 setAny name null element

setHandler :: EFn.EffectFn3 String (Event -> Effect Unit) Element Unit
setHandler = EFn.mkEffectFn3 \name handler element ->
  let listener = mkEventListener handler
   in EFn.runEffectFn3 setAny name listener element
