module Util exposing (icon)

import Color as SvgColor
import Svg exposing (Svg)
import Element exposing (..)

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
