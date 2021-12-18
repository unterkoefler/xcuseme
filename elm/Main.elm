module Main exposing (main)

import Api.Generated exposing (Event, EventType(..), Widget(..), widgetDecoder)
import Json.Decode
import Browser
import Calendar
import Colors
import Color as SvgColor
import Date exposing (Date)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Material.Icons.Maps exposing (directions_run, hotel)
import Material.Icons.Content exposing (add)
import Svg exposing (Svg)
import Task
import Time
import Url.Builder

type alias Model =
    { currentDate : Date
    , selectedDate : Date
    , monthIndex : Int
    , widget : WidgetModel
    }

type WidgetModel
    = EventModel Event
    | EventListModel (List Event)
    | EventCalendarModel (List Event)
    | ErrorModel String

type Msg
    = NoOp
    | ReceiveDate Date
    | DateSelected Date
    | UpdateMonthIndex Int

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ReceiveDate date ->
            ( { model | currentDate = date, selectedDate = date }
            , Cmd.none 
            )

        DateSelected date ->
            ( { model | selectedDate = date }
            , Cmd.none
            )

        UpdateMonthIndex idx ->
            let _ = Debug.log "monthIndex" idx
            in
            ( { model | monthIndex = idx }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    layout [ Font.size 16 ] <|
    case model.widget of 
        ErrorModel m ->
            paragraph [] [ text m ]
        EventModel e ->
            viewEvent e
        EventListModel events ->
            viewHome
                { eventsView = eventCards events
                , innerMenu = changeViewLink "cal" }
        EventCalendarModel events ->
            viewHome
                { eventsView = eventCalendar { events = events, currentDate = model.currentDate, selectedDate = model.selectedDate, monthIndex = model.monthIndex }
                , innerMenu = changeViewLink "list" }

viewHome : { eventsView : Element Msg, innerMenu : Element Msg } -> Element Msg
viewHome { eventsView, innerMenu } =
    column
        [ spacing 24
        , width fill
        , height fill
        ]
        [ column [ spacing 12, width fill ]
            [ logEventButton Exercise 
            , logEventButton Excuse
            ]
        , innerMenu
        , eventsView
        ]

logEventButton : EventType -> Element Msg
logEventButton eventType =
    -- TODO: disable if selected
    let
        eventTypeString =
            case eventType of
                Exercise ->
                    "Exercise"
                Excuse ->
                    "Excuse"
        url =
            Url.Builder.absolute
                [ "NewEvent" ]
                [ Url.Builder.string "eventType" (eventTypeString) ]
                -- TODO: add date params

        color =
            case eventType of
                Exercise ->
                    Colors.teal
                Excuse ->
                    Colors.red
    in
    link
        (fullWidthButtonAttrs color)
        { label = text <| (++) "Log " eventTypeString
        , url = url
        }

fullWidthButtonAttrs : Color -> List (Attribute Msg)
fullWidthButtonAttrs color =
    [ Border.rounded 12
    , Border.shadow
        { offset = ( 0.3, 0.7 ), size = 0.5, blur = 0.1, color = Colors.gray }
    , Background.color color
    , width fill
    , padding 20
    , Font.center
    ]

changeViewLink : String -> Element Msg
changeViewLink mode =
    let
        url = 
            Url.Builder.absolute [ "Events" ] [ Url.Builder.string "mode" mode ]
        lbl = 
            "view " ++ mode
    in
    row 
        [ width fill ]
        [ link
            [ alignRight
            , htmlAttribute <| Html.Attributes.attribute "data-turbolinks-preload" "false"
            ]
            { url = url 
            , label = 
                el
                    [ Font.color Colors.blue
                    , Font.underline
                    , Font.size 14
                    ]
                <| text lbl
        
            }
        ]

viewEvent : Event -> Element Msg
viewEvent event =
    let
        ( title, fontColor ) =
            case event.eventType of
                Excuse ->
                    ( "Excuse Details", Colors.red )

                Exercise ->
                    ( "Exercise Details", Colors.teal )
        titleEl =
            el
                [ Region.heading 2
                , Font.color fontColor
                ]
            <|
                text title

        editLink =
            link
                [ alignRight
                ]
                { label = el [] <| text "edit"
                , url = Url.Builder.absolute [ "EditEvent" ] [ Url.Builder.string "eventId" event.id ]
                } 
    in
    column
        [ spacing 12
        , width fill
        , paddingEach { left = 0, right = 12, top = 0, bottom = 0 }
        ]
        [ row [ width fill ] [ titleEl, editLink ]
        , paragraph [ Font.italic, Font.size 12 ] [ text <| Date.format "EEEE, MMMM d YYYY" <| eventToDate event ]
        , paragraph [ Font.size 14 ] [ text event.description ]
        ]

eventCalendar : { events: List Event, currentDate : Date, selectedDate : Date, monthIndex : Int } -> Element Msg
eventCalendar args =
    let
        { monthIndex, currentDate, selectedDate, events } = args
        viewingDate = 
            Date.add Date.Months monthIndex currentDate
        weeks =
            Calendar.fromDate Nothing viewingDate
        selectedEvent =
            eventForDay events selectedDate
    in
    column
        [ spacing 12
        , width fill
        ]
    <|
        ([ calendarNavRow { monthIndex = monthIndex, viewingDate = viewingDate }
         , weekDayLabels
         ]
          ++ List.map (showWeek args) weeks
          ++ [ calendarCard selectedEvent ]
        )

calendarCard : Maybe Event -> Element Msg
calendarCard maybeEvent =
    case maybeEvent of
        Nothing ->
            card 
                { action = Button Nothing
                , lead = icon add Colors.lightGrayForSvg 24
                , labelText = "Nothing logged for selected day" 
                }
        Just e ->
            eventCard e

calendarNavRow : { monthIndex : Int, viewingDate : Date } -> Element Msg
calendarNavRow { monthIndex, viewingDate } =
    row [ width fill ]
            [ Input.button [ alignLeft ] { label = text "prev", onPress = Just << UpdateMonthIndex <| monthIndex - 1 }
            , el [ centerX ] <| text <| Date.format "MMMM y" <| viewingDate
            , Input.button [ alignRight, paddingXY 12 0 ] { label = text "today", onPress = Just << UpdateMonthIndex <| 0 }
            , Input.button [ alignRight ] { label = text "next", onPress = Just << UpdateMonthIndex <| monthIndex + 1 }
            ]


weekDayLabels : Element Msg
weekDayLabels =
    let
        weekDays =
            [ "S", "M", "T", "W", "R", "F", "S" ]
    in
    row
        [ spacing 6, width fill ]
        (weekDays
            |> List.map
                (\lbl ->
                    calendarCell
                        { bgColor = Nothing
                        , shrinkHeight = True
                        , innerContent = el [ Font.size 14, Font.center, width fill ] <| text lbl
                        }
                )
        )

showWeek : { events : List Event, currentDate : Date, selectedDate : Date, monthIndex : Int } -> List Calendar.CalendarDate -> Element Msg
showWeek args days =
    row 
        [ spacing 6
        , width fill
        ]
    <| List.map (showDay args) days

showDay : { events : List Event, currentDate : Date, selectedDate : Date, monthIndex : Int } -> Calendar.CalendarDate -> Element Msg
showDay { events, currentDate, selectedDate } { dayDisplay, date } =
    let
        isFutureDate =
            Date.compare currentDate date == LT

        fontColor =
            case ( isFutureDate, dayDisplay ) of
                (True, _) ->
                    Colors.mediumGray

                ( _, " " ) ->
                    Colors.gray

                _ ->
                    Colors.black

        maybeEvent =
            eventForDay events date

        onPress = 
            if isFutureDate then
                Nothing
            else
                Just <| DateSelected date

        backgroundColor =
            calendarCellBackgroundColor 
                { currentDate = currentDate
                , selectedDate = selectedDate
                , maybeEvent =   maybeEvent
                , date =   date
                }


    in
    calendarCell
        { bgColor = backgroundColor
        , shrinkHeight = False
        , innerContent = 
            Input.button [ width fill, centerX, centerY ]
                { onPress = onPress
                , label =
                    el 
                        [ centerX
                        , centerY
                        , Font.color fontColor
                        ]
                    <|
                        text <|
                            String.fromInt <|
                                Date.day date
                }
        }



calendarCell : { bgColor : Maybe Color, shrinkHeight : Bool, innerContent : Element Msg } -> Element Msg
calendarCell { bgColor, shrinkHeight, innerContent } =
    -- TODO -> scale with screen size
    let
        fontSize = 12
        bgSize = 16
        bgAttrs =
            case bgColor of
                Just col ->
                    [ Background.color col
                    , Border.rounded 25 
                    ]
                Nothing ->
                    []

        attrs =
            [ width (px bgSize)
            , centerX
            , if shrinkHeight then
                height shrink
              else
                height (px bgSize)
            ]
            ++ bgAttrs
    in
    el
        [ width fill
        , centerX
        , Font.center
        , Font.size fontSize
        , paddingXY 6 0
        ]
    <|
        el
            attrs
            innerContent


eventForDay : List Event -> Date -> Maybe Event
eventForDay events date =
    case List.filter (isEventOnDate date) events of
        [] ->
            Nothing
        [ event ] ->
            Just event
        
        event :: others ->
            Just event

calendarCellBackgroundColor : { currentDate : Date, maybeEvent : Maybe Event, date :  Date, selectedDate : Date } -> Maybe Color
calendarCellBackgroundColor { currentDate, maybeEvent, date, selectedDate } =
    if selectedDate == date then
        Just Colors.blue
    else
        case (currentDate == date, maybeEvent) of
            (True, _) ->
                Just Colors.blueLighter
            (False, Nothing) ->
                Nothing
            (False, Just e) ->
                case e.eventType of
                    Exercise ->
                        Just Colors.teal
                    Excuse ->
                        Just Colors.red
                    

isEventOnDate : Date -> Event -> Bool
isEventOnDate date { year, month, day } =
    Date.year date == year && Date.monthNumber date == month && Date.day date == day


eventCards : List Event -> Element Msg
eventCards events =
    column
        [ width fill
        , scrollbarY
        , height <| minimum 400 <| fill
        ]
    <|
        List.map eventCard events

eventCard : Event -> Element Msg
eventCard event =
    let 
        txt : String 
        txt = (Date.format "M/d" <| eventToDate event)
                ++ " - "
                ++ event.description
        url : String 
        url = 
            Url.Builder.absolute [ "ShowEvent" ] [ Url.Builder.string "eventId" event.id ]

        (iconF, iconColor) =
            case event.eventType of
                Exercise ->
                    ( directions_run, Colors.tealForSvg )

                Excuse ->
                    ( hotel, Colors.redForSvg )

    in
    card { lead = icon iconF iconColor 24, labelText = txt, action = Link url }

type CardAction
    = Link String
    | Button (Maybe Msg)


card : { lead: Element Msg, labelText : String, action : CardAction } -> Element Msg
card { lead, labelText, action } =
    let
        maxLength = 35

        truncatedText =
            case String.length labelText > maxLength of
                False ->
                    labelText
                True ->
                    String.left (maxLength - 3) labelText ++ "..."

        label =
            row
                [ spacing 12
                , Border.widthEach { top = 1, bottom = 1, left = 0, right = 0 }
                , Border.color Colors.lightGray
                , paddingXY 0 6
                , width fill
                , Font.size 12
                ]
                [ lead
                , text truncatedText
                ]
    in
    case action of
        Link url ->
            link
                [ width fill ]
                { label = label
                , url = url
                }
        Button onPress ->
            Input.button
                [ width fill ]
                { label = label
                , onPress = onPress
                }


eventToDate : Event -> Date
eventToDate { year, month, day } =
    Date.fromCalendarDate
        year
        (Date.numberToMonth month)
        day

icon : (SvgColor.Color -> Int -> Svg msg) -> SvgColor.Color -> Int -> Element msg
icon iconF color size =
    el [ width (px size), height (px size) ] <|
        html <|
            Svg.svg
                []
                [ iconF color size ]


-- MAIN

main : Program Json.Decode.Value Model Msg
main =
    Browser.element 
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    ( { widget = initialWidgetModel flags
      , currentDate = Date.fromCalendarDate 1 Time.Nov 1995
      , selectedDate = Date.fromCalendarDate 1 Time.Nov 1995
      , monthIndex = 0
      }
    , Date.today |> Task.perform ReceiveDate
    )

initialWidgetModel : Json.Decode.Value -> WidgetModel
initialWidgetModel flags =
    case Json.Decode.decodeValue widgetDecoder flags of
        Ok widget ->
            widgetFlagToModel widget
        Err error ->
            ErrorModel (Json.Decode.errorToString error)

widgetFlagToModel : Widget -> WidgetModel
widgetFlagToModel widget =
    case widget of
        EventWidget event ->
            EventModel event
        EventListWidget events ->
            EventListModel events
        EventCalendarWidget events ->
            EventCalendarModel events
