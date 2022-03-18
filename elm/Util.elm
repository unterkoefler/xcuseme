module Util exposing (icon)

import Color as SvgColor
import Svg exposing (Svg)
import Element exposing (..)

icon : (SvgColor.Color -> Int -> Svg msg) -> SvgColor.Color -> Int -> Element msg
icon iconF color size =
    el [ width (px size), height (px size) ] <|
        html <|
            Svg.svg
                []
                [ iconF color size ]
