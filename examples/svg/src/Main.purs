module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Grain (VNode, mount)
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

view :: VNode
view =
  H.element "svg"
    # H.css svgStyles
    # H.prop "viewBox" "0 0 1000 600"
    # H.kids
        [ H.element "symbol" # H.id "s-text" # H.kids
            [ H.element "text"
                # H.prop "text-anchor" "middle"
                # H.prop "x" "50%"
                # H.prop "y" "68%"
                # H.kids [ H.text "Grain" ]
            ]
        , H.element "g" # H.kids
            [ H.element "use" # H.css useStyles # H.prop "xlink:href" "#s-text"
            , H.element "use" # H.css useStyles # H.prop "xlink:href" "#s-text"
            , H.element "use" # H.css useStyles # H.prop "xlink:href" "#s-text"
            , H.element "use" # H.css useStyles # H.prop "xlink:href" "#s-text"
            , H.element "use" # H.css useStyles # H.prop "xlink:href" "#s-text"
            ]
        ]
  where
    svgStyles =
      """
      body {
        margin: 0;
        background: #082330;
        background-size: .12em 100%;
      }
      .& {
        font: 16em/1 Arial;
        width: 100%;
        height: 100vh;
      }
      """
    useStyles =
      """
      .& {
        fill: none;
        stroke: white;
        stroke-dasharray: 7% 28%;
        stroke-width: 3px;
        animation: & 9s infinite cubic-bezier(.48,1.45,.86,1.15);
      }
      .&:nth-child(1) { stroke: #360745; stroke-dashoffset: 7%; }
      .&:nth-child(2) { stroke: #D61C59; stroke-dashoffset: 14%; }
      .&:nth-child(3) { stroke: #E7D84B; stroke-dashoffset: 21%; }
      .&:nth-child(4) { stroke: #EFEAC5; stroke-dashoffset: 28%; }
      .&:nth-child(5) { stroke: #1B8798; stroke-dashoffset: 35%; }
      @keyframes & {
        50% {
          stroke-dashoffset: 35%;
          stroke-dasharray: 0 87.5%;
        }
      }
      """
