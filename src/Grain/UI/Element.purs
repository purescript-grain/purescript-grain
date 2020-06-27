module Grain.UI.Element
  ( allocElement
  , updateElement
  ) where

import Prelude

import Effect (Effect)
import Foreign.Object (Object)
import Grain.Styler (Styler)
import Grain.UI.Handler (allocHandlers, updateHandlers)
import Grain.UI.Prop (allocProps, updateProps)
import Grain.UI.Util (createElementNS_, createElement_)
import Web.DOM.Element (Element)
import Web.Event.Event (Event)

type VElementPart r =
  { tagName :: String
  , props :: Object String
  , handlers :: Object (Event -> Effect Unit)
  | r
  }

type AllocArgs r =
  { isSvg :: Boolean
  , styler :: Styler
  , next :: VElementPart r
  }

allocElement :: forall r. AllocArgs r -> Effect Element
allocElement args = do
  el <- if args.isSvg
    then createElementNS_ args.next.tagName
    else createElement_ args.next.tagName
  allocProps
    { isSvg: args.isSvg
    , styler: args.styler
    , nexts: args.next.props
    , element: el
    }
  allocHandlers
    { nexts: args.next.handlers
    , element: el
    }
  pure el

type UpdateArgs r =
  { isSvg :: Boolean
  , styler :: Styler
  , current :: VElementPart r
  , next :: VElementPart r
  , element :: Element
  }

updateElement :: forall r. UpdateArgs r -> Effect Unit
updateElement args = do
  updateProps
    { isSvg: args.isSvg
    , styler: args.styler
    , currents: args.current.props
    , nexts: args.next.props
    , element: args.element
    }
  updateHandlers
    { currents: args.current.handlers
    , nexts: args.next.handlers
    , element: args.element
    }
