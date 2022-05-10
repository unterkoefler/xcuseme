module Api exposing
    ( createEvent
    , createUser
    , deleteEvent
    , login
    , logout
    , updateEvent
    )

import Api.Generated exposing (Event, EventType(..), User, eventDecoder, eventTypeEncoder, userDecoder)
import Http
import Json.Encode
import Urls


post_method =
    "POST"


delete_method =
    "DELETE"


createEvent : { event : Event, dateText : String } -> (Result Http.Error Event -> msg) -> Cmd msg
createEvent { event, dateText } onFinish =
    ihpRequest
        { method = post_method
        , headers = []
        , url = Urls.createEvent
        , body = eventWithDateStringEncoder event dateText |> Http.jsonBody
        , expect = Http.expectJson onFinish eventDecoder
        }


updateEvent : { event : Event, dateText : String } -> (Result Http.Error Event -> msg) -> Cmd msg
updateEvent { event, dateText } onFinish =
    ihpRequest
        { method = post_method
        , headers = []
        , url = Urls.updateEvent event.id
        , body = eventWithDateStringEncoder event dateText |> Http.jsonBody
        , expect = Http.expectJson onFinish eventDecoder
        }


deleteEvent : Event -> (Result Http.Error () -> msg) -> Cmd msg
deleteEvent event onFinish =
    ihpRequest
        { method = delete_method
        , headers = []
        , url = Urls.deleteEvent event.id
        , body = Http.emptyBody
        , expect = Http.expectWhatever onFinish
        }


logout : (Result Http.Error () -> msg) -> Cmd msg
logout onFinish =
    ihpRequest
        { method = delete_method
        , headers = []
        , url = Urls.deleteSession
        , body = Http.emptyBody
        , expect = Http.expectWhatever onFinish
        }


login : { email : String, password : String } -> (Result Http.Error () -> msg) -> Cmd msg
login credentials onFinish =
    ihpRequest
        { method = post_method
        , headers = [ Http.header "Accept" "text/html" ] -- TODO: don't do this
        , url = Urls.createSession
        , body = credentialsEncoder credentials |> Http.jsonBody
        , expect = Http.expectWhatever onFinish
        }


createUser : { email : String, password : String } -> (Result Http.Error User -> msg) -> Cmd msg
createUser credentials onFinish =
    ihpRequest
        { method = post_method
        , headers = []
        , url = Urls.createUser
        , body = credentialsEncoder credentials |> Http.jsonBody
        , expect = Http.expectJson onFinish userDecoder
        }


credentialsEncoder : { email : String, password : String } -> Json.Encode.Value
credentialsEncoder { email, password } =
    Json.Encode.object
        [ ( "email", Json.Encode.string email )
        , ( "password", Json.Encode.string password )
        ]


eventWithDateStringEncoder : Event -> String -> Json.Encode.Value
eventWithDateStringEncoder event dateText =
    Json.Encode.object
        [ ( "eventType", event.eventType |> eventTypeEncoder )
        , ( "date", Json.Encode.string dateText )
        , ( "description", Json.Encode.string event.description )
        ]


ihpRequest :
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , expect : Http.Expect msg
    }
    -> Cmd msg
ihpRequest { method, headers, url, body, expect } =
    Http.request
        { method = method
        , headers =
            [ Http.header "Accept" "application/json" ] ++ headers
        , url = url
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }
