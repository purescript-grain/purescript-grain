module Grain.UI.Element
  ( allocElement
  , updateElement
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Grain.Styler (Styler)
import Grain.UI.Handler (allocHandlers, updateHandlers)
import Grain.UI.Prop (allocProps, updateProps)
import Grain.UI.Util (createElementNS_, createElement_)
import Web.DOM.Element (Element)
import Web.Event.Event (Event)

type SpecialProps =
  { css :: Maybe String
  , className :: Maybe String
  }

type VElementPart r =
  { tagName :: String
  , props :: Array (Tuple String String)
  , handlers :: Array (Tuple String (Event -> Effect Unit))
  , specialProps :: SpecialProps
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
    , specialNexts: args.next.specialProps
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
    , specialCurrents: args.current.specialProps
    , specialNexts: args.next.specialProps
    , element: args.element
    }
  updateHandlers
    { currents: args.current.handlers
    , nexts: args.next.handlers
    , element: args.element
    }
