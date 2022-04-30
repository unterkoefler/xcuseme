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
    , Violation(..)
    , violationEncoder
    , violationDecoder
    , FlashMessage(..)
    , flashMessageEncoder
    , flashMessageDecoder
    , User
    , userEncoder
    , userDecoder
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
    | LoginWidget 
    | NewUserWidget 
    | FlashMessageWidget FlashMessage


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

        EditEventWidget b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "EditEventWidget")
            , ("contents" , eventEncoder b) ]

        AboutWidget ->
            Json.Encode.object [("tag" , Json.Encode.string "AboutWidget")]

        LoginWidget ->
            Json.Encode.object [("tag" , Json.Encode.string "LoginWidget")]

        NewUserWidget ->
            Json.Encode.object [("tag" , Json.Encode.string "NewUserWidget")]

        FlashMessageWidget b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "FlashMessageWidget")
            , ("contents" , flashMessageEncoder b) ]


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

        "EditEventWidget" ->
            Json.Decode.succeed EditEventWidget |>
            Json.Decode.Pipeline.required "contents" eventDecoder

        "AboutWidget" ->
            Json.Decode.succeed AboutWidget

        "LoginWidget" ->
            Json.Decode.succeed LoginWidget

        "NewUserWidget" ->
            Json.Decode.succeed NewUserWidget

        "FlashMessageWidget" ->
            Json.Decode.succeed FlashMessageWidget |>
            Json.Decode.Pipeline.required "contents" flashMessageDecoder

        _ ->
            Json.Decode.fail "No matching constructor")


type alias Event  =
    { id : String
    , eventType : EventType
    , description : String
    , day : Int
    , month : Int
    , year : Int
    , errors : List (String , Violation) }


eventEncoder : Event -> Json.Encode.Value
eventEncoder a =
    Json.Encode.object [ ("id" , Json.Encode.string a.id)
    , ("eventType" , eventTypeEncoder a.eventType)
    , ("description" , Json.Encode.string a.description)
    , ("day" , Json.Encode.int a.day)
    , ("month" , Json.Encode.int a.month)
    , ("year" , Json.Encode.int a.year)
    , ("errors" , Json.Encode.list (\b -> case b of
        (c , d) ->
            Json.Encode.list identity [ Json.Encode.string c
            , violationEncoder d ]) a.errors) ]


eventDecoder : Json.Decode.Decoder Event
eventDecoder =
    Json.Decode.succeed Event |>
    Json.Decode.Pipeline.required "id" Json.Decode.string |>
    Json.Decode.Pipeline.required "eventType" eventTypeDecoder |>
    Json.Decode.Pipeline.required "description" Json.Decode.string |>
    Json.Decode.Pipeline.required "day" Json.Decode.int |>
    Json.Decode.Pipeline.required "month" Json.Decode.int |>
    Json.Decode.Pipeline.required "year" Json.Decode.int |>
    Json.Decode.Pipeline.required "errors" (Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 Json.Decode.string) (Json.Decode.index 1 violationDecoder)))


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


type Violation 
    = TextViolation { message : String }
    | HtmlViolation { message : String }


violationEncoder : Violation -> Json.Encode.Value
violationEncoder a =
    case a of
        TextViolation b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "TextViolation")
            , ("message" , Json.Encode.string b.message) ]

        HtmlViolation b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "HtmlViolation")
            , ("message" , Json.Encode.string b.message) ]


violationDecoder : Json.Decode.Decoder Violation
violationDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "TextViolation" ->
            Json.Decode.map TextViolation (Json.Decode.succeed (\b -> { message = b }) |>
            Json.Decode.Pipeline.required "message" Json.Decode.string)

        "HtmlViolation" ->
            Json.Decode.map HtmlViolation (Json.Decode.succeed (\b -> { message = b }) |>
            Json.Decode.Pipeline.required "message" Json.Decode.string)

        _ ->
            Json.Decode.fail "No matching constructor")


type FlashMessage 
    = SuccessFlashMessage String
    | ErrorFlashMessage String


flashMessageEncoder : FlashMessage -> Json.Encode.Value
flashMessageEncoder a =
    case a of
        SuccessFlashMessage b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "SuccessFlashMessage")
            , ("contents" , Json.Encode.string b) ]

        ErrorFlashMessage b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "ErrorFlashMessage")
            , ("contents" , Json.Encode.string b) ]


flashMessageDecoder : Json.Decode.Decoder FlashMessage
flashMessageDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "SuccessFlashMessage" ->
            Json.Decode.succeed SuccessFlashMessage |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string

        "ErrorFlashMessage" ->
            Json.Decode.succeed ErrorFlashMessage |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string

        _ ->
            Json.Decode.fail "No matching constructor")


type alias User  =
    { errors : List (String , Violation) }


userEncoder : User -> Json.Encode.Value
userEncoder a =
    Json.Encode.object [ ("errors" , Json.Encode.list (\b -> case b of
        (c , d) ->
            Json.Encode.list identity [ Json.Encode.string c
            , violationEncoder d ]) a.errors) ]


userDecoder : Json.Decode.Decoder User
userDecoder =
    Json.Decode.succeed User |>
    Json.Decode.Pipeline.required "errors" (Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 Json.Decode.string) (Json.Decode.index 1 violationDecoder)))