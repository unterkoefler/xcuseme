module Api.Generated exposing
    ( Widget(..)
    , widgetEncoder
    , widgetDecoder
    , Event
    , eventEncoder
    , eventDecoder
    , EventType(..)
    , eventTypeEncoder
    , eventTypeDecoder
    , NavBarContext
    , navBarContextEncoder
    , navBarContextDecoder
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


widgetEncoder : Widget -> Json.Encode.Value
widgetEncoder a =
    case a of
        EventWidget b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "EventWidget")
            , ("contents" , eventEncoder b) ]

        EventListWidget b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "EventListWidget")
            , ("contents" , Json.Encode.list eventEncoder b) ]

        EventCalendarWidget b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "EventCalendarWidget")
            , ("contents" , Json.Encode.list eventEncoder b) ]

        NavBarWidget b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "NavBarWidget")
            , ("contents" , navBarContextEncoder b) ]

        NewEventWidget b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "NewEventWidget")
            , ("contents" , eventEncoder b) ]


widgetDecoder : Json.Decode.Decoder Widget
widgetDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "EventWidget" ->
            Json.Decode.succeed EventWidget |>
            Json.Decode.Pipeline.required "contents" eventDecoder

        "EventListWidget" ->
            Json.Decode.succeed EventListWidget |>
            Json.Decode.Pipeline.required "contents" (Json.Decode.list eventDecoder)

        "EventCalendarWidget" ->
            Json.Decode.succeed EventCalendarWidget |>
            Json.Decode.Pipeline.required "contents" (Json.Decode.list eventDecoder)

        "NavBarWidget" ->
            Json.Decode.succeed NavBarWidget |>
            Json.Decode.Pipeline.required "contents" navBarContextDecoder

        "NewEventWidget" ->
            Json.Decode.succeed NewEventWidget |>
            Json.Decode.Pipeline.required "contents" eventDecoder

        _ ->
            Json.Decode.fail "No matching constructor")


type alias Event  =
    { id : String
    , eventType : EventType
    , description : String
    , day : Int
    , month : Int
    , year : Int }


eventEncoder : Event -> Json.Encode.Value
eventEncoder a =
    Json.Encode.object [ ("id" , Json.Encode.string a.id)
    , ("eventType" , eventTypeEncoder a.eventType)
    , ("description" , Json.Encode.string a.description)
    , ("day" , Json.Encode.int a.day)
    , ("month" , Json.Encode.int a.month)
    , ("year" , Json.Encode.int a.year) ]


eventDecoder : Json.Decode.Decoder Event
eventDecoder =
    Json.Decode.succeed Event |>
    Json.Decode.Pipeline.required "id" Json.Decode.string |>
    Json.Decode.Pipeline.required "eventType" eventTypeDecoder |>
    Json.Decode.Pipeline.required "description" Json.Decode.string |>
    Json.Decode.Pipeline.required "day" Json.Decode.int |>
    Json.Decode.Pipeline.required "month" Json.Decode.int |>
    Json.Decode.Pipeline.required "year" Json.Decode.int


type EventType 
    = Exercise 
    | Excuse 


eventTypeEncoder : EventType -> Json.Encode.Value
eventTypeEncoder a =
    case a of
        Exercise ->
            Json.Encode.string "Exercise"

        Excuse ->
            Json.Encode.string "Excuse"


eventTypeDecoder : Json.Decode.Decoder EventType
eventTypeDecoder =
    Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "Exercise" ->
            Json.Decode.succeed Exercise

        "Excuse" ->
            Json.Decode.succeed Excuse

        _ ->
            Json.Decode.fail "No matching constructor")


type alias NavBarContext  =
    { loggedIn : Bool }


navBarContextEncoder : NavBarContext -> Json.Encode.Value
navBarContextEncoder a =
    Json.Encode.object [("loggedIn" , Json.Encode.bool a.loggedIn)]


navBarContextDecoder : Json.Decode.Decoder NavBarContext
navBarContextDecoder =
    Json.Decode.succeed NavBarContext |>
    Json.Decode.Pipeline.required "loggedIn" Json.Decode.bool