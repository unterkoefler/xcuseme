module Api exposing 
    ( createEvent
    , updateEvent
    , deleteEvent
    , logout
    )

import Api.Generated exposing (Event, EventType(..), eventDecoder, eventTypeEncoder)
import Http
import Json.Encode
import Urls

createEvent : { event : Event, dateText : String } -> (Result Http.Error Event -> msg) -> Cmd msg
createEvent { event, dateText } onFinish =
    ihpRequest
        { method = "POST"
        , headers = []
        , url = Urls.createEvent
        , body = eventWithDateStringEncoder event dateText |> Http.jsonBody
        , expect = Http.expectJson onFinish eventDecoder 
        }



updateEvent : { event : Event, dateText : String } -> (Result Http.Error Event -> msg) -> Cmd msg
updateEvent { event, dateText } onFinish =
    ihpRequest
        { method = "POST"
        , headers = []
        , url = Urls.updateEvent event.id
        , body = eventWithDateStringEncoder event dateText |> Http.jsonBody 
        , expect = Http.expectJson onFinish eventDecoder 
        }

deleteEvent : Event -> (Result Http.Error () -> msg ) -> Cmd msg
deleteEvent event onFinish =
    ihpRequest
        { method = "DELETE"
        , headers = []
        , url = Urls.deleteEvent event.id
        , body = Http.emptyBody
        , expect = Http.expectWhatever onFinish
        }


logout : (Result Http.Error () -> msg) -> Cmd msg
logout onFinish =
    ihpRequest
        { method = "DELETE"
        , headers = []
        , url = Urls.deleteSession
        , body = Http.emptyBody
        , expect = Http.expectWhatever onFinish
        }

eventWithDateStringEncoder : Event -> String -> Json.Encode.Value
eventWithDateStringEncoder event dateText  =
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

