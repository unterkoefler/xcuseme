module Util exposing (icon)

import Color as SvgColor
import Element exposing (..)
import Svg exposing (Svg)


icon : (SvgColor.Color -> Int -> Svg msg) -> Color -> Int -> Element msg
icon iconF color size =
    el [ width (px size), height (px size) ] <|
        html <|
            Svg.svg
                []
                [ iconF (elementColorToSvgColor color) size ]


elementColorToSvgColor : Color -> SvgColor.Color
elementColorToSvgColor =
    toRgb >> SvgColor.fromRgba
