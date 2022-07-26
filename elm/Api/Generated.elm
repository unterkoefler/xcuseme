module Api.Generated exposing
    ( Datum
    , Event
    , EventType(..)
    , FlashMessage(..)
    , NavBarContext
    , PlacedWord
    , Position
    , Statistics
    , User
    , Violation(..)
    , Widget(..)
    , datumDecoder
    , datumEncoder
    , eventDecoder
    , eventEncoder
    , eventTypeDecoder
    , eventTypeEncoder
    , flashMessageDecoder
    , flashMessageEncoder
    , navBarContextDecoder
    , navBarContextEncoder
    , placedWordDecoder
    , placedWordEncoder
    , positionDecoder
    , positionEncoder
    , statisticsController
    , statisticsEncoder
    , userDecoder
    , userEncoder
    , violationDecoder
    , violationEncoder
    , widgetDecoder
    , widgetEncoder
    )

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type Widget
    = EventWidget Event
    | EventListWidget (List Event)
    | EventCalendarWidget (List Event)
    | NavBarWidget NavBarContext
    | NewEventWidget Event
    | EditEventWidget Event
    | AboutWidget
    | StatsWidget Statistics
    | LoginWidget
    | NewUserWidget
    | FlashMessageWidget FlashMessage


widgetEncoder : Widget -> Json.Encode.Value
widgetEncoder a =
    case a of
        EventWidget b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "EventWidget" )
                , ( "contents", eventEncoder b )
                ]

        EventListWidget b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "EventListWidget" )
                , ( "contents", Json.Encode.list eventEncoder b )
                ]

        EventCalendarWidget b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "EventCalendarWidget" )
                , ( "contents", Json.Encode.list eventEncoder b )
                ]

        NavBarWidget b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "NavBarWidget" )
                , ( "contents", navBarContextEncoder b )
                ]

        NewEventWidget b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "NewEventWidget" )
                , ( "contents", eventEncoder b )
                ]

        EditEventWidget b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "EditEventWidget" )
                , ( "contents", eventEncoder b )
                ]

        AboutWidget ->
            Json.Encode.object [ ( "tag", Json.Encode.string "AboutWidget" ) ]

        StatsWidget b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "StatsWidget" )
                , ( "contents", statisticsEncoder b )
                ]

        LoginWidget ->
            Json.Encode.object [ ( "tag", Json.Encode.string "LoginWidget" ) ]

        NewUserWidget ->
            Json.Encode.object [ ( "tag", Json.Encode.string "NewUserWidget" ) ]

        FlashMessageWidget b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "FlashMessageWidget" )
                , ( "contents", flashMessageEncoder b )
                ]


widgetDecoder : Json.Decode.Decoder Widget
widgetDecoder =
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\a ->
                case a of
                    "EventWidget" ->
                        Json.Decode.succeed EventWidget
                            |> Json.Decode.Pipeline.required "contents" eventDecoder

                    "EventListWidget" ->
                        Json.Decode.succeed EventListWidget
                            |> Json.Decode.Pipeline.required "contents" (Json.Decode.list eventDecoder)

                    "EventCalendarWidget" ->
                        Json.Decode.succeed EventCalendarWidget
                            |> Json.Decode.Pipeline.required "contents" (Json.Decode.list eventDecoder)

                    "NavBarWidget" ->
                        Json.Decode.succeed NavBarWidget
                            |> Json.Decode.Pipeline.required "contents" navBarContextDecoder

                    "NewEventWidget" ->
                        Json.Decode.succeed NewEventWidget
                            |> Json.Decode.Pipeline.required "contents" eventDecoder

                    "EditEventWidget" ->
                        Json.Decode.succeed EditEventWidget
                            |> Json.Decode.Pipeline.required "contents" eventDecoder

                    "AboutWidget" ->
                        Json.Decode.succeed AboutWidget

                    "StatsWidget" ->
                        Json.Decode.succeed StatsWidget
                            |> Json.Decode.Pipeline.required "contents" statisticsController

                    "LoginWidget" ->
                        Json.Decode.succeed LoginWidget

                    "NewUserWidget" ->
                        Json.Decode.succeed NewUserWidget

                    "FlashMessageWidget" ->
                        Json.Decode.succeed FlashMessageWidget
                            |> Json.Decode.Pipeline.required "contents" flashMessageDecoder

                    _ ->
                        Json.Decode.fail "No matching constructor"
            )


type alias Event =
    { id : String
    , eventType : EventType
    , description : String
    , day : Int
    , month : Int
    , year : Int
    , errors : List ( String, Violation )
    }


eventEncoder : Event -> Json.Encode.Value
eventEncoder a =
    Json.Encode.object
        [ ( "id", Json.Encode.string a.id )
        , ( "eventType", eventTypeEncoder a.eventType )
        , ( "description", Json.Encode.string a.description )
        , ( "day", Json.Encode.int a.day )
        , ( "month", Json.Encode.int a.month )
        , ( "year", Json.Encode.int a.year )
        , ( "errors"
          , Json.Encode.list
                (\b ->
                    case b of
                        ( c, d ) ->
                            Json.Encode.list identity
                                [ Json.Encode.string c
                                , violationEncoder d
                                ]
                )
                a.errors
          )
        ]


eventDecoder : Json.Decode.Decoder Event
eventDecoder =
    Json.Decode.succeed Event
        |> Json.Decode.Pipeline.required "id" Json.Decode.string
        |> Json.Decode.Pipeline.required "eventType" eventTypeDecoder
        |> Json.Decode.Pipeline.required "description" Json.Decode.string
        |> Json.Decode.Pipeline.required "day" Json.Decode.int
        |> Json.Decode.Pipeline.required "month" Json.Decode.int
        |> Json.Decode.Pipeline.required "year" Json.Decode.int
        |> Json.Decode.Pipeline.required "errors" (Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 Json.Decode.string) (Json.Decode.index 1 violationDecoder)))


type EventType
    = Exercise
    | Excuse


eventTypeEncoder : EventType -> Json.Encode.Value
eventTypeEncoder a =
    case a of
        Exercise ->
            Json.Encode.string "exercise"

        Excuse ->
            Json.Encode.string "excuse"


eventTypeDecoder : Json.Decode.Decoder EventType
eventTypeDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\a ->
                case a of
                    "Exercise" ->
                        Json.Decode.succeed Exercise

                    "Excuse" ->
                        Json.Decode.succeed Excuse

                    _ ->
                        Json.Decode.fail "No matching constructor"
            )


type alias NavBarContext =
    { loggedIn : Bool }


navBarContextEncoder : NavBarContext -> Json.Encode.Value
navBarContextEncoder a =
    Json.Encode.object [ ( "loggedIn", Json.Encode.bool a.loggedIn ) ]


navBarContextDecoder : Json.Decode.Decoder NavBarContext
navBarContextDecoder =
    Json.Decode.succeed NavBarContext
        |> Json.Decode.Pipeline.required "loggedIn" Json.Decode.bool


type Violation
    = TextViolation { message : String }
    | HtmlViolation { message : String }


violationEncoder : Violation -> Json.Encode.Value
violationEncoder a =
    case a of
        TextViolation b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "TextViolation" )
                , ( "message", Json.Encode.string b.message )
                ]

        HtmlViolation b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "HtmlViolation" )
                , ( "message", Json.Encode.string b.message )
                ]


violationDecoder : Json.Decode.Decoder Violation
violationDecoder =
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\a ->
                case a of
                    "TextViolation" ->
                        Json.Decode.map TextViolation
                            (Json.Decode.succeed (\b -> { message = b })
                                |> Json.Decode.Pipeline.required "message" Json.Decode.string
                            )

                    "HtmlViolation" ->
                        Json.Decode.map HtmlViolation
                            (Json.Decode.succeed (\b -> { message = b })
                                |> Json.Decode.Pipeline.required "message" Json.Decode.string
                            )

                    _ ->
                        Json.Decode.fail "No matching constructor"
            )


type FlashMessage
    = SuccessFlashMessage String
    | ErrorFlashMessage String


flashMessageEncoder : FlashMessage -> Json.Encode.Value
flashMessageEncoder a =
    case a of
        SuccessFlashMessage b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "SuccessFlashMessage" )
                , ( "contents", Json.Encode.string b )
                ]

        ErrorFlashMessage b ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "ErrorFlashMessage" )
                , ( "contents", Json.Encode.string b )
                ]


flashMessageDecoder : Json.Decode.Decoder FlashMessage
flashMessageDecoder =
    Json.Decode.field "tag" Json.Decode.string
        |> Json.Decode.andThen
            (\a ->
                case a of
                    "SuccessFlashMessage" ->
                        Json.Decode.succeed SuccessFlashMessage
                            |> Json.Decode.Pipeline.required "contents" Json.Decode.string

                    "ErrorFlashMessage" ->
                        Json.Decode.succeed ErrorFlashMessage
                            |> Json.Decode.Pipeline.required "contents" Json.Decode.string

                    _ ->
                        Json.Decode.fail "No matching constructor"
            )


type alias User =
    { errors : List ( String, Violation ) }


userEncoder : User -> Json.Encode.Value
userEncoder a =
    Json.Encode.object
        [ ( "errors"
          , Json.Encode.list
                (\b ->
                    case b of
                        ( c, d ) ->
                            Json.Encode.list identity
                                [ Json.Encode.string c
                                , violationEncoder d
                                ]
                )
                a.errors
          )
        ]


userDecoder : Json.Decode.Decoder User
userDecoder =
    Json.Decode.succeed User
        |> Json.Decode.Pipeline.required "errors" (Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 Json.Decode.string) (Json.Decode.index 1 violationDecoder)))


type alias Statistics =
    { excuseCount : Int
    , exerciseCount : Int
    , currentExerciseStreak : Int
    , longestExerciseStreak : Int
    , frequentExcuses : List ( String, Int )
    , cloud : List PlacedWord
    }


statisticsEncoder : Statistics -> Json.Encode.Value
statisticsEncoder a =
    Json.Encode.object
        [ ( "excuseCount", Json.Encode.int a.excuseCount )
        , ( "exerciseCount", Json.Encode.int a.exerciseCount )
        , ( "currentExerciseStreak", Json.Encode.int a.currentExerciseStreak )
        , ( "longestExerciseStreak", Json.Encode.int a.longestExerciseStreak )
        , ( "frequentExcuses"
          , Json.Encode.list
                (\b ->
                    case b of
                        ( c, d ) ->
                            Json.Encode.list identity
                                [ Json.Encode.string c
                                , Json.Encode.int d
                                ]
                )
                a.frequentExcuses
          )
        , ( "cloud", Json.Encode.list placedWordEncoder a.cloud )
        ]


statisticsController : Json.Decode.Decoder Statistics
statisticsController =
    Json.Decode.succeed Statistics
        |> Json.Decode.Pipeline.required "excuseCount" Json.Decode.int
        |> Json.Decode.Pipeline.required "exerciseCount" Json.Decode.int
        |> Json.Decode.Pipeline.required "currentExerciseStreak" Json.Decode.int
        |> Json.Decode.Pipeline.required "longestExerciseStreak" Json.Decode.int
        |> Json.Decode.Pipeline.required "frequentExcuses" (Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 Json.Decode.string) (Json.Decode.index 1 Json.Decode.int)))
        |> Json.Decode.Pipeline.required "cloud" (Json.Decode.list placedWordDecoder)


type alias PlacedWord =
    { word : Datum, position : Position }


placedWordEncoder : PlacedWord -> Json.Encode.Value
placedWordEncoder a =
    Json.Encode.object
        [ ( "word", datumEncoder a.word )
        , ( "position", positionEncoder a.position )
        ]


placedWordDecoder : Json.Decode.Decoder PlacedWord
placedWordDecoder =
    Json.Decode.succeed PlacedWord
        |> Json.Decode.Pipeline.required "word" datumDecoder
        |> Json.Decode.Pipeline.required "position" positionDecoder


type alias Position =
    { x : Float, y : Float }


positionEncoder : Position -> Json.Encode.Value
positionEncoder a =
    Json.Encode.object
        [ ( "x", Json.Encode.float a.x )
        , ( "y", Json.Encode.float a.y )
        ]


positionDecoder : Json.Decode.Decoder Position
positionDecoder =
    Json.Decode.succeed Position
        |> Json.Decode.Pipeline.required "x" Json.Decode.float
        |> Json.Decode.Pipeline.required "y" Json.Decode.float


type alias Datum =
    { count : Int, label : String, fontSize : Int, width : Int, height : Int }


datumEncoder : Datum -> Json.Encode.Value
datumEncoder a =
    Json.Encode.object
        [ ( "count", Json.Encode.int a.count )
        , ( "label", Json.Encode.string a.label )
        , ( "fontSize", Json.Encode.int a.fontSize )
        , ( "width", Json.Encode.int a.width )
        , ( "height", Json.Encode.int a.height )
        ]


datumDecoder : Json.Decode.Decoder Datum
datumDecoder =
    Json.Decode.succeed Datum
        |> Json.Decode.Pipeline.required "count" Json.Decode.int
        |> Json.Decode.Pipeline.required "label" Json.Decode.string
        |> Json.Decode.Pipeline.required "fontSize" Json.Decode.int
        |> Json.Decode.Pipeline.required "width" Json.Decode.int
        |> Json.Decode.Pipeline.required "height" Json.Decode.int
