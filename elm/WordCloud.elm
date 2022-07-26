module WordCloud exposing (wordCloud)

import Api.Generated exposing (PlacedWord)
import Colors
import Dict exposing (Dict, get, update)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region


wordCloud : List PlacedWord -> Element msg
wordCloud placedWords =
    case placedWords of
        [] ->
            paragraph [] [ text "There isn't enough data to make a word cloud. Go make more excuses." ]

        _ ->
            column
                [ spacing 0
                , width fill
                ]
                (wordCloudHelp placedWords 0 0.0)


wordCloudHelp : List PlacedWord -> Int -> Float -> List (Element msg)
wordCloudHelp placedWords index heightAcc =
    case placedWords of
        [] ->
            []

        x :: xs ->
            wordToElem index x heightAcc :: wordCloudHelp xs (index + 1) (heightAcc + toFloat x.word.height)


wordToElem : Int -> PlacedWord -> Float -> Element msg
wordToElem idx { word, position } heightAcc =
    el
        [ Font.size word.fontSize
        , height (px word.height)
        , width (px word.width)
        , moveRight <| (position.x |> bringToCenterX)
        , moveDown <| (position.y |> negateColumnOffset heightAcc |> bringToCenterY)
        , Font.center
        , Element.centerY
        , Font.family [ Font.monospace ]
        ]
        (text word.label)


centerX =
    200


centerY =
    150


bringToCenterX : Float -> Float
bringToCenterX =
    (+) centerX


bringToCenterY : Float -> Float
bringToCenterY =
    (+) centerY


negateColumnOffset : Float -> Float -> Float
negateColumnOffset heightAcc =
    (+) (heightAcc * -1)


a : Float
a =
    0.02


b : Float
b =
    0.02


spiralX : Float -> Float -> Float
spiralX i =
    (+) (a * i * cos i)


spiralY : Float -> Float -> Float
spiralY i =
    (+) (b * i * sin i)



-- + 800 / (i + 1) / (i + 1))


xOffset : Int -> Float
xOffset idx =
    let
        i =
            toFloat idx
    in
    0
        |> spiralX i
        |> bringToCenterX


yOffset : Int -> Float
yOffset idx =
    let
        i =
            toFloat idx
    in
    0
        |> spiralY i
        |> negateColumnOffset i
        |> bringToCenterY
