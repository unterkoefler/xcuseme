module Main exposing (main)

import Api.Generated exposing (Event, EventType(..), Widget(..), widgetDecoder, NavBarContext, Violation(..), eventDecoder)
import Api
import Json.Decode
import Browser
import Browser.Navigation
import Calendar
import Colors
import Color as SvgColor
import Date exposing (Date)
import DatePicker
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Http
import Material.Icons.Maps exposing (directions_run, hotel)
import Material.Icons.Content exposing (add)
import Svg exposing (Svg)
import Task
import Time
import Urls

type alias Model =
    { currentDate : Date
    , selectedDate : Date
    , monthIndex : Int
    , useDarkMode : Bool
    , widget : WidgetModel
    , pickerModel : DatePicker.Model
    , pickerDateText : String
    , showLogEventModal : Bool
    , flashMessage : Maybe FlashMessage
    , showMenu : Bool
    }

type alias FlashMessage =
    { messageType : FlashMessageType
    , message : String
    }

type FlashMessageType
    = Success
    | Error
    | Info

type WidgetModel
    = EventModel Event
    | EventListModel (List Event)
    | EventCalendarModel (List Event)
    | NavBarModel NavBarContext
    | ErrorModel String
    | NewEventModel Event
    | EditEventModel Event

type Msg
    = NoOp
    | ReceiveDate Date
    | DateSelected Date
    | UpdateMonthIndex Int
    | UpdateEventDescription String
    | UpdateEventDate DatePicker.ChangeEvent
    | CreateEvent
    | EventCreated (Result Http.Error Event)
    | UpdateEvent
    | EventUpdated (Result Http.Error Event)
    | DeleteEvent Event
    | EventDeleted (Result Http.Error ())
    | Logout
    | LoggedOut (Result Http.Error ())
    | ToggleLogEventModal
    | SetFlashMessage (Maybe FlashMessage)
    | ToggleMenu

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ReceiveDate date ->
            ( { model 
                | currentDate = date
                , selectedDate = date 
                , pickerModel = DatePicker.setToday date model.pickerModel
                , pickerDateText = 
                    if model.pickerDateText == "" then
                        Date.format "Y-MM-dd" date
                    else
                        model.pickerDateText
              }
            , Cmd.none 
            )

        DateSelected date ->
            ( { model | selectedDate = date, flashMessage = Nothing }
            , Cmd.none
            )

        UpdateMonthIndex idx ->
            ( { model | monthIndex = idx, flashMessage = Nothing }
            , Cmd.none
            )

        UpdateEventDescription description ->
            case model.widget of
                NewEventModel event ->
                    ( { model 
                        | widget = NewEventModel { event | description = description } 
                        , flashMessage = Nothing
                    }
                    , Cmd.none
                    )

                EditEventModel event ->
                    ( { model 
                        | widget = EditEventModel { event | description = description} 
                        , flashMessage = Nothing
                    }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateEventDate change ->
            case model.widget of
                NewEventModel event ->
                    ( updateEventDate model event change, Cmd.none )
                EditEventModel event ->
                    ( updateEventDate model event change, Cmd.none )
                _ ->
                    ( model, Cmd.none )
        CreateEvent ->
            case model.widget of
                NewEventModel event ->
                    ( { model | flashMessage = Nothing }
                    , Api.createEvent { event = event, dateText =model.pickerDateText }
                            EventCreated
                    )
                _ ->
                    ( model, Cmd.none )

        EventCreated (Err e) ->
            let
                errorMsg = httpErrorToString e
            in
            ( { model | flashMessage = Just { messageType = Error, message = errorMsg } }
            , Cmd.none 
            )

        EventCreated (Ok event) ->
            case event.errors of
                [] ->
                    ( model, Browser.Navigation.load Urls.root )

                _ ->
                    ( { model | widget = NewEventModel event }
                    , Cmd.none 
                    )
        UpdateEvent ->
            case model.widget of
                EditEventModel event ->
                    ( model, Api.updateEvent { event = event, dateText =  model.pickerDateText } EventUpdated )
                _ ->
                    ( model, Cmd.none )

        EventUpdated (Err e) ->
            let
                errorMsg = httpErrorToString e
            in
            ( { model | flashMessage = Just { messageType = Error, message = errorMsg } }
            , Cmd.none 
            )

        EventUpdated (Ok event) ->
            case event.errors of
                [] ->
                    ( model
                    , Browser.Navigation.load <| Urls.showEvent event.id )

                _ ->
                    ( { model | widget = EditEventModel event }
                    , Cmd.none
                    )

        DeleteEvent event ->
            ( { model | flashMessage = Nothing }
            , Api.deleteEvent event EventDeleted
            )

        EventDeleted (Err e) ->
            let
                errorMsg = httpErrorToString e
            in
            ( { model | flashMessage = Just { messageType = Error, message = errorMsg } }
            , Cmd.none 
            )

        EventDeleted (Ok ()) ->
            ( model
            , Browser.Navigation.load Urls.root
            )

        Logout ->
            ( model
            , Api.logout LoggedOut
            )

        LoggedOut (Err e) ->
            let
                errorMsg = httpErrorToString e
            in
            ( { model | flashMessage = Just { messageType = Error, message = errorMsg } }
            , Cmd.none 
            )

        LoggedOut (Ok _) ->
            ( model, Browser.Navigation.load Urls.newSession )

        ToggleLogEventModal ->
            ( { model | showLogEventModal = not model.showLogEventModal, flashMessage = Nothing }
            , Cmd.none
            )

        ToggleMenu ->
            ( { model | showMenu = not model.showMenu }
            , Cmd.none
            )

        SetFlashMessage fm ->
            ( { model | flashMessage = fm }
            , Cmd.none
            )


httpErrorToString : Http.Error -> String
httpErrorToString e =
    case e of
        Http.BadBody s ->
            "Error: BadBody " ++ s

        Http.Timeout ->
            "Error: Request timed out."

        Http.BadUrl s ->
            "Error: BadUrl " ++ s

        Http.NetworkError ->
            "Error: Internet broke"

        Http.BadStatus i ->
            "Error: BadStatus " ++ (String.fromInt i)

updateEventDate : Model -> Event -> DatePicker.ChangeEvent -> Model
updateEventDate model event change =
    case change of
        DatePicker.DateChanged date ->
            updateEventDateAndClosePicker model event date
        DatePicker.PickerChanged subMsg ->
            { model | pickerModel = model.pickerModel |> DatePicker.update subMsg }

        DatePicker.TextChanged newText ->
            case parseDate newText of
                Just date ->
                    updateEventDateAndClosePicker model event date
                Nothing ->
                    { model | pickerDateText = newText }

parseDate : String -> Maybe Date
parseDate str =
    str 
        |> Date.fromIsoString
        |> Result.toMaybe

updateEventDateAndClosePicker : Model -> Event -> Date -> Model
updateEventDateAndClosePicker model event date =
    let
        newEvent = 
            { event 
                | year = Date.year date
                , month = Date.monthNumber date
                , day = Date.day date
            }

        newWidget = 
            case model.widget of
                NewEventModel _ ->
                    NewEventModel newEvent

                EditEventModel _ ->
                    EditEventModel newEvent

                _ ->
                    -- something went wrong
                    model.widget
    in
    { model
        | widget = newWidget
        , pickerModel = model.pickerModel |> DatePicker.close
        , pickerDateText = Date.format "Y-MM-dd" date
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    let
       (bgColor, fontColor) =
            case model.useDarkMode of
                True ->
                    (Colors.black, Colors.white)
                False ->
                    (Colors.white, Colors.black)

       options = 
           case model.widget of
               NavBarModel _ ->
                   []
               _ ->
                   [ noStaticStyleSheet ]
    in
    layoutWith { options = options }
         [ nunito, Font.size 16, Background.color bgColor ] <|
    case model.widget of 
        ErrorModel m ->
            paragraph [] [ text m ]
        EventModel e ->
            viewEvent e
        NavBarModel context ->
            navBar context model.showMenu
        EventListModel events ->
            viewHome
                { eventsView = eventCards events
                , innerMenu = changeViewLink "cal" 
                , selectedDate = model.selectedDate
                , events = events
                , flashMessage = model.flashMessage
                }
        EventCalendarModel events ->
            viewHome
                { eventsView = eventCalendar 
                    { events = events
                    , currentDate = model.currentDate
                    , selectedDate = model.selectedDate
                    , monthIndex = model.monthIndex
                    , useDarkMode = model.useDarkMode 
                    , showLogEventModal = model.showLogEventModal
                    }
                , innerMenu = changeViewLink "list" 
                , selectedDate = model.selectedDate
                , events = events
                , flashMessage = model.flashMessage
                }
        NewEventModel event ->
            eventForm 
                { event = event
                , pickerModel = model.pickerModel
                , currentDate = model.currentDate
                , pickerDateText = model.pickerDateText 
                , onSave = CreateEvent
                , deleteButton = Element.none
                }

        EditEventModel event ->
            eventForm 
                { event = event
                , pickerModel = model.pickerModel
                , currentDate = model.currentDate
                , pickerDateText = model.pickerDateText 
                , onSave = UpdateEvent
                , deleteButton = deleteEventButton event
                }


nunito : Attribute msg
nunito =
    Font.family
        [ Font.typeface "Nunito"
        , Font.serif
        ]

viewHome : 
    { eventsView : Element Msg
    , innerMenu : Element Msg
    , selectedDate : Date 
    , events : List Event
    , flashMessage : Maybe FlashMessage
    } -> Element Msg
viewHome { eventsView, innerMenu, selectedDate, events, flashMessage } =
    column
        [ spacing 24
        , width fill
        , height fill
        , paddingXY 48 0
        ]
        [ flashMessage |> Maybe.map viewFlashMessage |> Maybe.withDefault Element.none
        , column [ spacing 12, width fill ]
            [ logEventButton Exercise selectedDate events
            , logEventButton Excuse selectedDate events
            ]
        , innerMenu
        , eventsView
        ]

navBar : NavBarContext -> Bool -> Element Msg
navBar { loggedIn } showMenu  =
    let
        menu =
            if showMenu then
                menuOptions loggedIn
            else
                Element.none
    in
    column [ width fill ]
    [ row 
        [ width fill
        , padding 16
        , Background.color Colors.indigo
        ]
        [ link 
            [ Region.heading 1
            , Font.size 28
            , centerX
            , width fill
            , Font.center
            , Font.color Colors.white
            , paddingEach { top = 0, left = 24, right = 0, bottom = 0 }
            ]
            { label = text "XcuseMe"
            , url = Urls.root
            }
        , menuButton
        ]
    , menu
    ]

menuButton :  Element Msg
menuButton =
     Input.button 
        [ Font.size 28
        , Font.color Colors.white
        , alignRight
        , width (px 24)

        ]
        { label = text "⋮"
        , onPress = Just ToggleMenu
        }

menuOptions : Bool -> Element Msg
menuOptions loggedIn =
    let
        logoutButton : Element Msg
        logoutButton = 
            case loggedIn of
                True ->
                    Input.button menuItemAttrs
                        { label = text "Logout" 
                        , onPress = Just Logout
                        }
                False ->
                    Element.none
    in
    column
        [ Font.size 18
        , width fill
        , Background.color Colors.indigoDarker
        , Font.color Colors.white
        , paddingXY 12 0
        ]
    <|
        borderBetween
           [ link menuItemAttrs { url = Urls.root, label = text "About"}
           , logoutButton
           ] 

menuItemAttrs : List (Attribute Msg)
menuItemAttrs =
    [ paddingXY 0 24
    , Font.alignRight
    , width fill
    ]

borderBetween : List (Element msg) -> List (Element msg)
borderBetween elements =
    case elements of
        [] ->
            []

        [ element ] ->
            [ element ]

        element :: rest ->
            el [ Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }, width fill ]
                element
                :: borderBetween rest
        

logEventButton : EventType -> Date -> List Event -> Element Msg
logEventButton eventType selectedDate events =
    -- TODO: disable if there is an existing event
    let
        eventTypeString =
            eventType |> eventTypeToString |> firstCharToUpper
        url =
            Urls.newEvent { eventType = eventTypeString, date = selectedDate }
        color =
            eventTypeToColor eventType

        event : Maybe Event
        event =
            eventForDay events selectedDate
    in
    case event of
        Nothing ->
            link
                (fullWidthButtonAttrs color)
                { label = text <| (++) "Log " eventTypeString
                , url = url
                }
        Just _ ->
            Input.button
                (fullWidthButtonAttrs Colors.lightGray)
                { label = text <| (++) "Log " eventTypeString
                , onPress = 
                    { message = "You can log only one exercise or excuse per day"
                    , messageType = Info
                    } |> Just |> SetFlashMessage |> Just 
                }

eventTypeToString : EventType -> String
eventTypeToString eventType =
    case eventType of
        Exercise ->
            "exercise"
        Excuse ->
            "excuse"

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



eventTypeToColor : EventType -> Color
eventTypeToColor eventType =
   case eventType of
       Exercise ->
           Colors.teal
       Excuse ->
           Colors.red


logEventButtonForModal : EventType -> Date -> List (Attribute Msg) -> Element Msg
logEventButtonForModal eventType selectedDate borderAttrs =
    let
        eventTypeString =
            eventType |> eventTypeToString |> firstCharToUpper
        url =
            Urls.newEvent { eventType = eventTypeString, date = selectedDate }
        color =
            eventTypeToColor eventType
    in
    link
        ([ Font.color color
        , Background.color Colors.white
        , width fill
        , Font.center
        , centerX
        , paddingXY 0 12
        ] ++ borderAttrs)
        { label = text <| (++) "Log " eventTypeString
        , url = url
        }

changeViewLink : String -> Element Msg
changeViewLink mode =
    let
        url = 
            Urls.events mode
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
        title = 
            event.eventType |> eventTypeToString |> firstCharToUpper
        fontColor = 
            event.eventType |> eventTypeToColor
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
                , url = Urls.editEvent event.id
                } 
    in
    column
        [ spacing 12
        , width fill
        , paddingXY 48 0
        ]
        [ row [ width fill ] [ titleEl, editLink ]
        , paragraph [ Font.italic, Font.size 12 ] [ text <| Date.format "EEEE, MMMM d YYYY" <| eventToDate event ]
        , paragraph [ Font.size 14 ] [ text event.description ]
        ]

eventCalendar : 
    { events: List Event
    , currentDate : Date
    , selectedDate : Date
    ,monthIndex : Int
    , useDarkMode : Bool 
    , showLogEventModal : Bool
    } -> Element Msg
eventCalendar args =
    let
        { monthIndex, currentDate, selectedDate, events, showLogEventModal } = args
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
          ++ [ calendarCard selectedEvent showLogEventModal selectedDate ]
        )

calendarCard : Maybe Event -> Bool -> Date -> Element Msg
calendarCard maybeEvent showLogEventModal selectedDate =
    case maybeEvent of
        Nothing ->
            let
                modalAttrs : List (Attribute Msg)
                modalAttrs =
                    case showLogEventModal of
                        True ->
                            [ logEventModal selectedDate |> above ]

                        False ->
                            []

            in
            el
                ([ width fill ] ++ modalAttrs)
                <| card 
                    { action = Button (Just ToggleLogEventModal)
                    , lead = icon add Colors.lightGrayForSvg 24
                    , labelText = "Nothing logged for selected day" 
                    }
        Just e ->
            eventCard e

logEventModal : Date -> Element Msg
logEventModal selectedDate =
    column
        [ width fill ]
        [ logEventButtonForModal Exercise selectedDate 
            [ Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
            , Border.color Colors.gray 
            ]
        , logEventButtonForModal Excuse selectedDate []
        ]

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

showWeek : { a | events : List Event, currentDate : Date, selectedDate : Date, monthIndex : Int, useDarkMode : Bool } -> List Calendar.CalendarDate -> Element Msg
showWeek args days =
    row 
        [ spacing 6
        , width fill
        ]
    <| List.map (showDay args) days

showDay : { a | events : List Event, currentDate : Date, selectedDate : Date, monthIndex : Int, useDarkMode : Bool } -> Calendar.CalendarDate -> Element Msg
showDay { events, currentDate, selectedDate, useDarkMode } { dayDisplay, date } =
    let
        isFutureDate =
            Date.compare currentDate date == LT

        fontColor =
            case ( isFutureDate, dayDisplay ) of
                (True, _) ->
                    Colors.mediumGray

                ( _, "  " ) ->
                    if useDarkMode then Colors.lightGray else Colors.gray

                _ ->
                    if useDarkMode then Colors.white else Colors.black

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
                e.eventType |> eventTypeToColor |> Just
                    

isEventOnDate : Date -> Event -> Bool
isEventOnDate date { year, month, day } =
    Date.year date == year && Date.monthNumber date == month && Date.day date == day


eventCards : List Event -> Element Msg
eventCards events =
    case events of
        [] ->
            paragraph [ Font.center, Font.italic, width fill ]
                [ text "Nothing logged yet" ]
        _ ->
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
            Urls.showEvent event.id

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

eventForm : 
    { event : Event
    , pickerModel : DatePicker.Model
    , currentDate : Date
    , pickerDateText : String 
    , onSave : Msg
    , deleteButton : Element Msg
    } -> Element Msg
eventForm { event, pickerModel, currentDate, pickerDateText, onSave, deleteButton } =
    let
        defaultSettings =
            DatePicker.defaultSettings
        datePickerSettings =
            { defaultSettings
                | disabled = \day -> Date.compare currentDate day == LT
                , firstDayOfWeek = Time.Sun
            }
        (eventTypeStr, saveButtonColor) =
            case event.eventType of
                Excuse ->
                    ("Excuse", Colors.red )
                Exercise ->
                    ("Exercise", Colors.teal )
        
        descriptionError = 
            errorMessageForField event.errors "description"

        dateError =
            errorMessageForField event.errors "date"
    in
    column
        [ spacing 24
        , width fill
        , paddingXY 48 0
        ]
        [ row
            [ spacing 24
            , width fill
            ]
            [ DatePicker.input []
                { onChange = UpdateEventDate
                , selected = Just <| eventToDate event
                , text = pickerDateText
                , label = Input.labelLeft [] <| text "Date:"
                , placeholder = Nothing
                , model = pickerModel
                , settings = datePickerSettings
                }
            , deleteButton
            ]
        , formError dateError
        , Input.multiline []
            { onChange = UpdateEventDescription
            , text = event.description
            , placeholder = Nothing
            , label = Input.labelAbove [] <| text "What's going on?"
            , spellcheck = False
            }
        , formError descriptionError
        , row 
            [ spacing 12
            , width fill
            ]
            [ link
                [ alignLeft
                , Border.rounded 4
                , Border.color Colors.darkRed
                , Border.width 1
                , paddingXY 24 12
                , Font.color Colors.darkRed
                ]
                { label = text "Cancel"
                , url = Urls.root
                }
            , Input.button
                [ alignRight
                , Border.rounded 4
                , Border.color Colors.black
                , Border.width 1
                , paddingXY 24 12
                , Background.color saveButtonColor
                ]
                { label = text <| "Save " ++ eventTypeStr
                , onPress = Just onSave
                }
            ]
        ]


deleteEventButton : Event -> Element Msg
deleteEventButton event =
    Input.button
        []
        { label = text "delete"
        , onPress = event |> DeleteEvent |> Just
        }

errorMessageForField : List (String, Violation) -> String -> Maybe String
errorMessageForField errors field =
    errors
        |> List.filter (\(f, violation) -> f == field)
        |> List.head
        |> Maybe.map 
            (\(fieldName, violation) ->
                case violation of
                    TextViolation { message } -> message
                    HtmlViolation { message } -> message
            )

formError : Maybe String -> Element Msg
formError maybeError =
    case maybeError of
      Nothing ->
          Element.none
      
      Just message  ->
          if String.startsWith "non_unique_date" message then
            nonUniqueDateMessage message
          else
            paragraph 
                [ Font.italic
                , Font.color Colors.darkRed
                ]
                [ text message ]

nonUniqueDateMessage : String -> Element Msg
nonUniqueDateMessage message =
    let
        eventId : String
        eventId =
            message 
                |> String.dropLeft (String.length "non_unique_date:")
    in
    paragraph
        [ Font.italic
        , Font.color Colors.darkRed
        ]
        [ text "You have already logged something for this date. Do you want to edit it instead? "
        , link
            [ Font.color Colors.blue
            , Font.underline
            ]
            { url = Urls.editEvent eventId
            , label = text "Edit existing event"
            }
        ]


viewFlashMessage : FlashMessage -> Element Msg
viewFlashMessage { messageType, message } =
    let
        bgColor : Color
        bgColor =
            case messageType of
                Success ->
                    Colors.success

                Info ->
                    Colors.info

                Error ->
                    Colors.error
    in
    paragraph
        [ Background.color bgColor
        , Font.color Colors.white
        , padding 6
        ]
        [ text message
        ]

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
    let
        widget = initialWidgetModel flags
        pickerDateText =
            case widget of
                NewEventModel event ->
                    Date.format "Y-MM-dd" <| eventToDate event

                EditEventModel event ->
                    Date.format "Y-MM-dd" <| eventToDate event
                _ ->
                    ""
    in
    ( { widget = initialWidgetModel flags
      , currentDate = Date.fromCalendarDate 1 Time.Nov 1995
      , selectedDate = Date.fromCalendarDate 1 Time.Nov 1995
      , useDarkMode = .useDarkMode (decodeExtra flags)
      , monthIndex = 0
      , pickerModel = DatePicker.init
      , pickerDateText = pickerDateText
      , showLogEventModal = False
      , flashMessage = Nothing
      , showMenu = False
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

decodeExtra : Json.Decode.Value -> { useDarkMode : Bool }
decodeExtra flags =
    case Json.Decode.decodeValue (Json.Decode.field "useDarkMode" Json.Decode.bool) flags of
        Ok v ->
            { useDarkMode = v }
        Err error ->
            { useDarkMode = False }

widgetFlagToModel : Widget -> WidgetModel
widgetFlagToModel widget =
    case widget of
        EventWidget event ->
            EventModel event
        EventListWidget events ->
            EventListModel events
        EventCalendarWidget events ->
            EventCalendarModel events
        NavBarWidget context ->
            NavBarModel context
        NewEventWidget event ->
            NewEventModel event
        EditEventWidget event ->
            EditEventModel event



firstCharToUpper : String -> String
firstCharToUpper s =
    (s |> String.left 1 |> String.toUpper) ++
    (s |> String.dropLeft 1)
