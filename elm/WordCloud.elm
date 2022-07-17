module WordCloud exposing (wordCloud)

import Colors
import Dict exposing (Dict, get, update)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region


maxFontSize : Int
maxFontSize =
    48


wordCloud : List ( String, Int ) -> Element msg
wordCloud tuples =
    let
        sortedData =
            List.sortBy Tuple.second tuples |> List.reverse

        maxD : Int
        maxD =
            sortedData |> List.head |> Maybe.withDefault ( "", 1 ) |> Tuple.second

        data : List Datum
        data =
            List.map (datumFromTuple maxD) sortedData

        placedWords =
            placeWords data |> List.reverse
    in
    column
        [ spacing 0 ]
        (List.indexedMap wordToElem placedWords)


wordCloudV1 : List ( String, Int ) -> Element msg
wordCloudV1 data =
    let
        sortedData =
            List.sortBy Tuple.second data |> List.reverse

        maxD : Float
        maxD =
            data |> List.head |> Maybe.withDefault ( "", 1 ) |> Tuple.second |> toFloat

        normalizedData =
            List.map (\( lbl, c ) -> ( lbl, toFloat c / maxD )) sortedData
    in
    column
        [ spacing 0 ]
        (List.indexedMap wordV1 normalizedData)


wordToElem : Int -> PlacedWord -> Element msg
wordToElem idx { word, position } =
    el
        [ Font.size word.fontSize
        , height (px word.height)
        , width (px word.width)
        , clipX -- TODO: this shouldnt be necessary if word.width is correct
        , clipY -- TODO: this shouldnt be necessary if word.height is correct
        , moveRight <| (position.x |> bringToCenterX)
        , moveDown <| (position.y |> negateColumnOffset (toFloat idx) |> bringToCenterY)
        ]
        (text word.label)


wordV1 : Int -> ( String, Float ) -> Element msg
wordV1 idx ( label, normalizedValue ) =
    el
        [ Font.size (toFloat maxFontSize * normalizedValue |> floor)
        , height (px maxFontSize)
        , moveRight <| xOffset idx -- <| cos (toFloat idx) * -80 + 300
        , moveDown <| yOffset idx -- sin (toFloat idx) * 50.0 + toFloat maxFontSize * toFloat idx - 200
        ]
        (text label)


centerX =
    300


centerY =
    300


bringToCenterX : Float -> Float
bringToCenterX =
    (+) centerX


bringToCenterY : Float -> Float
bringToCenterY =
    (+) centerY


negateColumnOffset : Float -> Float -> Float
negateColumnOffset i =
    (+) (toFloat maxFontSize * i * -1)


a : Float
a =
    0.02


b : Float
b =
    0.02


spiralX : Float -> Float -> Float
spiralX i =
    (+) (a * i * cos i)



--+ (max 0 (800 - 200 * i)))


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


type alias Datum =
    { count : Int
    , label : String
    , fontSize : Int
    , width : Int
    , height : Int
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias PlacedWord =
    { word : Datum
    , position : Position
    }


datumFromTuple : Int -> ( String, Int ) -> Datum
datumFromTuple maxCount ( label, count ) =
    let
        normalizedValue : Float
        normalizedValue =
            toFloat count / toFloat maxCount

        fontSize : Int
        fontSize =
            toFloat maxFontSize * normalizedValue |> floor

        fontCharWidth : Int
        fontCharWidth =
            fontSize

        -- TODO: verify this
    in
    { count = count
    , label = label
    , fontSize = fontSize
    , width = String.length label * fontCharWidth
    , height = maxFontSize
    }


placeWords : List Datum -> List PlacedWord
placeWords data =
    placeWordsHelp data [] 0


placeWordsHelp : List Datum -> List PlacedWord -> Int -> List PlacedWord
placeWordsHelp data placedWords index =
    case data of
        [] ->
            placedWords

        x :: xs ->
            let
                ( placedWord, nextIndex ) =
                    placeWord x placedWords index
            in
            placeWordsHelp
                xs
                (placedWord :: placedWords)
                nextIndex


placeWord : Datum -> List PlacedWord -> Int -> ( PlacedWord, Int )
placeWord datum placedWords index =
    let
        x =
            0 |> spiralX (toFloat index)

        y =
            0 |> spiralY (toFloat index)

        placedWord =
            { word = datum, position = { x = x, y = y } }
    in
    if overlap placedWord placedWords then
        placeWord datum placedWords (index + 1)

    else
        ( placedWord, index + 1 )


overlap : PlacedWord -> List PlacedWord -> Bool
overlap target field =
    List.any (overlap1 target) field


overlap1 : PlacedWord -> PlacedWord -> Bool
overlap1 pwA pwB =
    let
        wordA =
            pwA.word

        posA =
            pwA.position

        wordB =
            pwB.word

        posB =
            pwB.position

        leftA =
            posA.x

        rightA =
            posA.x + toFloat wordA.width

        topA =
            posA.y

        bottomA =
            posA.y + toFloat wordA.height

        leftB =
            posB.x

        rightB =
            posB.x + toFloat wordB.width

        topB =
            posB.y

        bottomB =
            posB.y + toFloat wordB.height
    in
    leftA
        < rightB
        && rightA
        > leftB
        && topA
        < bottomB
        && bottomA
        > topB
