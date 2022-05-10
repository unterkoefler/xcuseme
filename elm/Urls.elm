module Urls exposing
    ( about
    , createEvent
    , createSession
    , createUser
    , deleteEvent
    , deleteSession
    , editEvent
    , events
    , newEvent
    , newSession
    , newUser
    , root
    , showEvent
    , updateEvent
    )

import Date exposing (Date)
import Url.Builder


root : String
root =
    Url.Builder.absolute [] []


about : String
about =
    Url.Builder.absolute [ "About" ] []


deleteSession : String
deleteSession =
    Url.Builder.absolute [ "DeleteSession" ] []


newSession : String
newSession =
    Url.Builder.absolute [ "NewSession" ] []


createSession : String
createSession =
    Url.Builder.absolute [ "CreateSession" ] []


newUser =
    Url.Builder.absolute [ "NewUser" ] []


createUser =
    Url.Builder.absolute [ "CreateUser" ] []


events : String -> String
events mode =
    Url.Builder.absolute [ "Events" ] [ Url.Builder.string "mode" mode ]


newEvent :
    { eventType : String
    , date : Date
    }
    -> String
newEvent { eventType, date } =
    Url.Builder.absolute
        [ "NewEvent" ]
        [ Url.Builder.string "eventType" eventType
        , Date.year date |> Url.Builder.int "year"
        , Date.monthNumber date |> Url.Builder.int "month"
        , Date.day date |> Url.Builder.int "day"
        ]


showEvent : String -> String
showEvent id =
    Url.Builder.absolute [ "ShowEvent" ] [ eventIdParam id ]


editEvent : String -> String
editEvent id =
    Url.Builder.absolute [ "EditEvent" ] [ eventIdParam id ]


createEvent : String
createEvent =
    Url.Builder.absolute [ "CreateEvent" ] []


deleteEvent : String -> String
deleteEvent id =
    Url.Builder.absolute [ "DeleteEvent" ] [ eventIdParam id ]


updateEvent : String -> String
updateEvent id =
    Url.Builder.absolute [ "UpdateEvent" ] [ eventIdParam id ]


eventIdParam : String -> Url.Builder.QueryParameter
eventIdParam =
    Url.Builder.string "eventId"
