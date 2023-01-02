module Main exposing (main)

import AboutPage
import Api
import Api.Generated
    exposing
        ( Event
        , EventType(..)
        , FlashMessage(..)
        , NavBarContext
        , Statistics
        , User
        , Violation(..)
        , Widget(..)
        , eventDecoder
        , widgetDecoder
        )
import Browser
import Browser.Navigation
import Chart
import Color as SvgColor
import Colors
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
import Json.Decode
import Material.Icons.Action exposing (check_circle)
import Material.Icons.Alert exposing (error_outline)
import Material.Icons.Content exposing (add)
import Material.Icons.Maps exposing (directions_run, hotel)
import RelativeDate
import Svg exposing (Svg)
import Task
import Time
import Urls
import Util exposing (icon)
import WordCloud exposing (wordCloud)
import XCalendar


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
    , isEventSaving : Bool
    , isEventDeleting : Bool
    }


type WidgetModel
    = EventModel Event
    | EventListModel (List Event)
    | EventCalendarModel (List Event)
    | NavBarModel NavBarContext
    | ErrorModel String
    | NewEventModel Event
    | EditEventModel Event
    | AboutModel
    | StatsModel Statistics
    | FlashMessageModel FlashMessage
    | LoginModel { email : String, password : String }
    | NewUserModel { email : String, password : String, errors : List ( String, Violation ) }


initLoginModel =
    { email = "", password = "" }


initNewUserModel =
    { email = "", password = "", errors = [] }


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
    | CreateUser
    | UserCreated (Result Http.Error User)
    | Login
    | LoggedIn (Result Http.Error ())
    | Logout
    | LoggedOut (Result Http.Error ())
    | ToggleLogEventModal
    | SetFlashMessage (Maybe FlashMessage)
    | ToggleMenu
    | UpdateEmail String
    | UpdatePassword String


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
                        Date.format "yyyy-MM-dd" date

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
                        | widget = EditEventModel { event | description = description }
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
                    ( { model | flashMessage = Nothing, isEventSaving = True }
                    , Api.createEvent { event = event, dateText = model.pickerDateText }
                        EventCreated
                    )

                _ ->
                    ( model, Cmd.none )

        EventCreated (Err e) ->
            let
                errorMsg =
                    httpErrorToString e
            in
            ( { model | flashMessage = ErrorFlashMessage errorMsg |> Just, isEventSaving = False }
            , Cmd.none
            )

        EventCreated (Ok event) ->
            case event.errors of
                [] ->
                    ( { model | isEventSaving = False }, Browser.Navigation.load Urls.root )

                _ ->
                    ( { model | widget = NewEventModel event, isEventSaving = False }
                    , Cmd.none
                    )

        UpdateEvent ->
            case model.widget of
                EditEventModel event ->
                    ( { model | isEventSaving = True }, Api.updateEvent { event = event, dateText = model.pickerDateText } EventUpdated )

                _ ->
                    ( model, Cmd.none )

        EventUpdated (Err e) ->
            let
                errorMsg =
                    httpErrorToString e
            in
            ( { model | flashMessage = ErrorFlashMessage errorMsg |> Just, isEventSaving = False }
            , Cmd.none
            )

        EventUpdated (Ok event) ->
            case event.errors of
                [] ->
                    ( { model | isEventSaving = False }
                    , Browser.Navigation.load <| Urls.showEvent event.id
                    )

                _ ->
                    ( { model | widget = EditEventModel event, isEventSaving = False }
                    , Cmd.none
                    )

        DeleteEvent event ->
            ( { model | flashMessage = Nothing, isEventDeleting = True }
            , Api.deleteEvent event EventDeleted
            )

        EventDeleted (Err e) ->
            let
                errorMsg =
                    httpErrorToString e
            in
            ( { model | flashMessage = ErrorFlashMessage errorMsg |> Just, isEventDeleting = False }
            , Cmd.none
            )

        EventDeleted (Ok ()) ->
            ( { model | isEventDeleting = False }
            , Browser.Navigation.load Urls.root
            )

        Logout ->
            ( model
            , Api.logout LoggedOut
            )

        LoggedOut (Err e) ->
            let
                errorMsg =
                    httpErrorToString e
            in
            ( { model | flashMessage = ErrorFlashMessage errorMsg |> Just }
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

        UpdateEmail newEmail ->
            case model.widget of
                LoginModel data ->
                    ( { model | widget = LoginModel { data | email = newEmail } }
                    , Cmd.none
                    )

                NewUserModel data ->
                    ( { model | widget = NewUserModel { data | email = newEmail } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdatePassword newPassword ->
            case model.widget of
                LoginModel data ->
                    ( { model | widget = LoginModel { data | password = newPassword } }
                    , Cmd.none
                    )

                NewUserModel data ->
                    ( { model | widget = NewUserModel { data | password = newPassword } }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Login ->
            case model.widget of
                LoginModel data ->
                    ( model
                    , Api.login data LoggedIn
                    )

                _ ->
                    ( model, Cmd.none )

        LoggedIn (Err e) ->
            let
                errorMsg =
                    httpErrorToString e
            in
            ( { model | flashMessage = ErrorFlashMessage errorMsg |> Just }
            , Cmd.none
            )

        LoggedIn (Ok _) ->
            ( { model | flashMessage = SuccessFlashMessage "you're in" |> Just }
            , Browser.Navigation.load Urls.root
            )

        CreateUser ->
            case model.widget of
                NewUserModel { email, password } ->
                    ( model
                    , Api.createUser { email = email, password = password } UserCreated
                    )

                _ ->
                    ( model, Cmd.none )

        UserCreated (Err e) ->
            let
                errorMsg =
                    httpErrorToString e
            in
            ( { model | flashMessage = ErrorFlashMessage errorMsg |> Just }
            , Cmd.none
            )

        UserCreated (Ok user) ->
            case user.errors of
                [] ->
                    ( model
                    , Browser.Navigation.load Urls.root
                    )

                _ ->
                    case model.widget of
                        NewUserModel data ->
                            ( { model | widget = NewUserModel { data | errors = user.errors } }
                            , Cmd.none
                            )

                        _ ->
                            ( { model | flashMessage = ErrorFlashMessage "Something unexpected happened. Please refresh the page." |> Just }
                            , Cmd.none
                            )


httpErrorToString : Http.Error -> String
httpErrorToString e =
    case e of
        Http.BadBody s ->
            "Error: " ++ s

        Http.Timeout ->
            "Error: Request timed out."

        Http.BadUrl s ->
            "Error: BadUrl " ++ s

        Http.NetworkError ->
            "Error: Internet broke"

        Http.BadStatus i ->
            "Error: BadStatus " ++ String.fromInt i


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
    if String.length str < 10 then
        Nothing

    else
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
        , pickerDateText = Date.format "yyyy-MM-dd" date
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        ( bgColor, fontColor ) =
            case model.useDarkMode of
                True ->
                    ( Colors.black, Colors.white )

                False ->
                    ( Colors.white, Colors.black )

        options =
            case model.widget of
                NavBarModel _ ->
                    []

                _ ->
                    [ noStaticStyleSheet ]
    in
    layoutWith { options = options }
        [ nunito, Font.size 16, Background.color bgColor ]
    <|
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
                    { eventsView =
                        eventCalendar
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
                    , isEventSaving = model.isEventSaving
                    }

            EditEventModel event ->
                eventForm
                    { event = event
                    , pickerModel = model.pickerModel
                    , currentDate = model.currentDate
                    , pickerDateText = model.pickerDateText
                    , onSave = UpdateEvent
                    , deleteButton = deleteEventButton { isEventDeleting = model.isEventDeleting } event
                    , isEventSaving = model.isEventSaving
                    }

            AboutModel ->
                AboutPage.view

            StatsModel statistics ->
                viewStats statistics

            FlashMessageModel flashMessage ->
                viewFlashMessage flashMessage

            LoginModel data ->
                loginForm data { flashMessage = model.flashMessage }

            NewUserModel data ->
                newUserForm data { flashMessage = model.flashMessage }


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
    }
    -> Element Msg
viewHome { eventsView, innerMenu, selectedDate, events, flashMessage } =
    column
        [ spacing 24
        , width fill
        , height fill
        ]
        [ flashMessage |> Maybe.map viewFlashMessage |> Maybe.withDefault Element.none
        , column [ spacing 12, width fill, paddingXY 24 0 ]
            [ logEventButton Exercise selectedDate events
            , logEventButton Excuse selectedDate events
            ]
        , innerMenu
        , eventsView
        ]


navBar : NavBarContext -> Bool -> Element Msg
navBar { loggedIn } showMenu =
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
            [ column
                [ spacing 4
                , centerX
                , width fill
                , paddingEach { top = 0, left = 24, right = 0, bottom = 0 }
                ]
                [ link
                    [ Region.heading 1
                    , Font.size 28
                    , centerX
                    , Font.center
                    , Font.color Colors.white
                    ]
                    { label = text "XcuseMe"
                    , url = Urls.root
                    }
                , paragraph [ Font.size 10, Font.italic, Font.center ] [ text "Exercise tracking for real people" ]
                ]
            , menuButton
            ]
        , menu
        ]


menuButton : Element Msg
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
        borderBetween Colors.white
            [ link menuItemAttrs { url = Urls.about, label = text "About" }
            , link menuItemAttrs { url = Urls.stats, label = text "Stats" }
            , logoutButton
            ]


menuItemAttrs : List (Attribute Msg)
menuItemAttrs =
    [ paddingXY 0 24
    , Font.alignRight
    , width fill
    ]


borderBetween : Color -> List (Element msg) -> List (Element msg)
borderBetween color elements =
    case elements of
        [] ->
            []

        [ element ] ->
            [ element ]

        element :: rest ->
            el
                [ Border.widthEach { top = 0, left = 0, right = 0, bottom = 1 }
                , width fill
                , Border.color color
                ]
                element
                :: borderBetween color rest


logEventButton : EventType -> Date -> List Event -> Element Msg
logEventButton eventType selectedDate events =
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

        labelText : String
        labelText =
            "Log " ++ eventTypeString
    in
    case event of
        Nothing ->
            link
                (fullWidthButtonAttrs color)
                { label = text labelText
                , url = url
                }

        Just _ ->
            Input.button
                (fullWidthButtonAttrs Colors.lightGray)
                { label = text labelText
                , onPress =
                    ErrorFlashMessage "You can log only one exercise or excuse per day"
                        |> Just
                        |> SetFlashMessage
                        |> Just
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


eventTypeToLighterColor : EventType -> Color
eventTypeToLighterColor eventType =
    case eventType of
        Exercise ->
            Colors.tealLighter

        Excuse ->
            Colors.redLighter


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
         ]
            ++ borderAttrs
        )
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
        [ width fill
        , paddingEach { left = 0, right = 24, top = 0, bottom = 0 }
        ]
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
                <|
                    text lbl
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
        , paragraph [ Font.italic, Font.size 12 ] [ text <| Date.format "EEEE MMMM d, yyyy" <| eventToDate event ]
        , paragraph [ Font.size 14 ] [ text event.description ]
        ]


eventCalendar :
    { events : List Event
    , currentDate : Date
    , selectedDate : Date
    , monthIndex : Int
    , useDarkMode : Bool
    , showLogEventModal : Bool
    }
    -> Element Msg
eventCalendar { monthIndex, currentDate, selectedDate, events, showLogEventModal, useDarkMode } =
    let
        selectedEvent =
            eventForDay events selectedDate
    in
    column
        [ width fill
        , spacing 6
        ]
        [ XCalendar.view
            { currentDate = currentDate
            , monthIndex = monthIndex
            , selectedDate = selectedDate
            , useDarkMode = useDarkMode
            , onDateSelected = DateSelected
            , onUpdateMonthIndex = UpdateMonthIndex
            , cellColor = calendarCellColors events
            }
        , calendarCard selectedEvent showLogEventModal selectedDate
        ]


calendarCellColors :
    List Event
    ->
        { currentDate : Date
        , date : Date
        , selectedDate : Date
        , dayDisplay : String
        }
    -> { backgroundColor : Maybe Color, fontColor : Color }
calendarCellColors events { currentDate, date, selectedDate, dayDisplay } =
    let
        maybeEvent =
            eventForDay events date

        isFutureDate =
            Date.compare currentDate date == LT

        fontColorDefault =
            case ( isFutureDate, dayDisplay ) of
                ( True, _ ) ->
                    Colors.mediumGray

                ( _, "  " ) ->
                    Colors.gray

                _ ->
                    Colors.black
    in
    if selectedDate == date then
        { backgroundColor = Just Colors.blue
        , fontColor = Colors.white
        }

    else
        case ( currentDate == date, maybeEvent ) of
            ( True, _ ) ->
                { backgroundColor = Just Colors.blueLighter
                , fontColor = Colors.white
                }

            ( False, Nothing ) ->
                { backgroundColor = Nothing
                , fontColor = fontColorDefault
                }

            ( False, Just e ) ->
                { backgroundColor = e.eventType |> eventTypeToLighterColor |> Just
                , fontColor = fontColorDefault
                }


calendarCard : Maybe Event -> Bool -> Date -> Element Msg
calendarCard maybeEvent showLogEventModal selectedDate =
    case maybeEvent of
        Nothing ->
            let
                modalAttrs : List (Attribute Msg)
                modalAttrs =
                    case showLogEventModal of
                        True ->
                            [ logEventModal selectedDate |> below ]

                        False ->
                            []
            in
            el
                ([ width fill, Border.widthXY 0 1, Border.color Colors.lightGray ] ++ modalAttrs)
            <|
                card
                    { action = Button (Just ToggleLogEventModal)
                    , lead = icon add Colors.lightGray 24
                    , labelText = "Nothing logged for selected day"
                    }

        Just e ->
            el [ width fill, Border.widthXY 0 1, Border.color Colors.lightGray ] <|
                eventCard e


logEventModal : Date -> Element Msg
logEventModal selectedDate =
    column
        [ width fill ]
        [ logEventButtonForModal Exercise
            selectedDate
            [ Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
            , Border.color Colors.gray
            ]
        , logEventButtonForModal Excuse selectedDate []
        ]


eventForDay : List Event -> Date -> Maybe Event
eventForDay events date =
    case List.filter (isEventOnDate date) events of
        [] ->
            Nothing

        [ event ] ->
            Just event

        event :: others ->
            Just event


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
                borderBetween Colors.lightGray <|
                    List.map eventCard events


eventCard : Event -> Element Msg
eventCard event =
    let
        txt : String
        txt =
            (Date.format "M/d" <| eventToDate event)
                ++ " - "
                ++ event.description

        url : String
        url =
            Urls.showEvent event.id

        ( iconF, iconColor ) =
            case event.eventType of
                Exercise ->
                    ( directions_run, Colors.teal )

                Excuse ->
                    ( hotel, Colors.red )
    in
    card { lead = icon iconF iconColor 24, labelText = txt, action = Link url }


type CardAction
    = Link String
    | Button (Maybe Msg)


card : { lead : Element Msg, labelText : String, action : CardAction } -> Element Msg
card { lead, labelText, action } =
    let
        maxLength =
            35

        truncatedText =
            case String.length labelText > maxLength of
                False ->
                    labelText

                True ->
                    String.left (maxLength - 3) labelText ++ "..."

        label =
            row
                [ spacing 12
                , paddingXY 12 6
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
    , isEventSaving : Bool
    }
    -> Element Msg
eventForm { event, pickerModel, currentDate, pickerDateText, onSave, deleteButton, isEventSaving } =
    let
        defaultSettings =
            DatePicker.defaultSettings

        datePickerSettings =
            { defaultSettings
                | disabled = \day -> Date.compare currentDate day == LT
                , firstDayOfWeek = Time.Sun
            }

        ( eventTypeStr, saveButtonColor ) =
            case event.eventType of
                Excuse ->
                    ( "Excuse", Colors.red )

                Exercise ->
                    ( "Exercise", Colors.teal )

        descriptionError =
            errorMessageForField event.errors "description"

        dateError =
            errorMessageForField event.errors "date"

        saveButtonLabel : String
        saveButtonLabel =
            if isEventSaving then
                "Saving..."

            else
                "Save " ++ eventTypeStr
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
        , relativeDateInfo { dateText = pickerDateText, currentDate = currentDate }
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
                { label = text saveButtonLabel
                , onPress = Just onSave
                }
            ]
        ]


relativeDateInfo : { dateText : String, currentDate : Date } -> Element Msg
relativeDateInfo { dateText, currentDate } =
    case parseDate dateText of
        Nothing ->
            Element.none

        Just date ->
            RelativeDate.toString { today = currentDate, other = date }
                |> text
                |> List.singleton
                |> paragraph [ Font.italic, Font.size 14 ]


loginForm :
    { email : String, password : String }
    -> { flashMessage : Maybe FlashMessage }
    -> Element Msg
loginForm { email, password } { flashMessage } =
    column
        [ spacing 24
        , paddingEach { left = 32, right = 0, top = 0, bottom = 0 }
        ]
        [ flashMessage |> Maybe.map viewFlashMessage |> Maybe.withDefault Element.none
        , Input.email
            []
            { label = Input.labelAbove [] <| text "Email"
            , text = email
            , placeholder = Nothing
            , onChange = UpdateEmail
            }
        , Input.currentPassword
            []
            { label = Input.labelAbove [] <| text "Password"
            , text = password
            , onChange = UpdatePassword
            , placeholder = Nothing
            , show = False
            }
        , Input.button
            [ Border.rounded 4
            , Border.color Colors.black
            , Border.width 1
            , paddingXY 24 12
            , Background.color Colors.indigo
            ]
            { label = text "Login"
            , onPress = Just Login
            }
        , paragraph
            [ Font.size 14 ]
            [ text "New here? "
            , link
                [ Font.underline
                , Font.color Colors.blue
                ]
                { url = Urls.newUser
                , label = text "Create an account "
                }
            , text "or "
            , link
                [ Font.underline
                , Font.color Colors.blue
                ]
                { url = Urls.about
                , label = text "learn more."
                }
            ]
        ]


newUserForm :
    { email : String, password : String, errors : List ( String, Violation ) }
    -> { flashMessage : Maybe FlashMessage }
    -> Element Msg
newUserForm { email, password, errors } { flashMessage } =
    let
        emailError =
            errorMessageForField errors "email"

        passwordError =
            errorMessageForField errors "password"
    in
    column
        [ spacing 24
        , paddingEach { left = 32, right = 0, top = 0, bottom = 0 }
        ]
        [ flashMessage |> Maybe.map viewFlashMessage |> Maybe.withDefault Element.none
        , Input.email
            []
            { label = Input.labelAbove [] <| text "Email"
            , text = email
            , placeholder = Nothing
            , onChange = UpdateEmail
            }
        , formError emailError
        , Input.newPassword
            []
            { label = Input.labelAbove [] <| text "Password"
            , text = password
            , onChange = UpdatePassword
            , placeholder = Nothing
            , show = False
            }
        , formError passwordError
        , Input.button
            [ Border.rounded 4
            , Border.color Colors.black
            , Border.width 1
            , paddingXY 24 12
            , Background.color Colors.indigo
            ]
            { label = text "Create Account"
            , onPress = Just CreateUser
            }
        , column
            [ spacing 14 ]
            [ paragraph
                [ Font.size 14 ]
                [ text "Been here before? "
                , link
                    [ Font.underline
                    , Font.color Colors.blue
                    ]
                    { url = Urls.newSession
                    , label = text "Login."
                    }
                ]
            , paragraph
                [ Font.size 14 ]
                [ text "Confused? "
                , link
                    [ Font.underline
                    , Font.color Colors.blue
                    ]
                    { url = Urls.about
                    , label = text "Learn more."
                    }
                ]
            ]
        ]


deleteEventButton : { isEventDeleting : Bool } -> Event -> Element Msg
deleteEventButton { isEventDeleting } event =
    let
        lbl : String
        lbl =
            if isEventDeleting then
                "deleting..."

            else
                "delete"
    in
    Input.button
        []
        { label = text lbl
        , onPress = event |> DeleteEvent |> Just
        }


errorMessageForField : List ( String, Violation ) -> String -> Maybe String
errorMessageForField errors field =
    errors
        |> List.filter (\( f, violation ) -> f == field)
        |> List.head
        |> Maybe.map
            (\( fieldName, violation ) ->
                case violation of
                    TextViolation { message } ->
                        message

                    HtmlViolation { message } ->
                        message
            )


formError : Maybe String -> Element Msg
formError maybeError =
    case maybeError of
        Nothing ->
            Element.none

        Just message ->
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
viewFlashMessage flashMessage =
    let
        ( message, color ) =
            case flashMessage of
                SuccessFlashMessage m ->
                    ( m, Colors.success )

                ErrorFlashMessage m ->
                    ( m, Colors.error )

        iconFunction =
            case flashMessage of
                SuccessFlashMessage _ ->
                    check_circle

                ErrorFlashMessage _ ->
                    error_outline
    in
    el
        [ paddingXY 24 12 ]
    <|
        row
            [ Border.width 2
            , Border.color color
            , Border.rounded 6
            , paddingXY 12 6
            , spacing 12
            ]
            [ icon iconFunction color 24
            , paragraph [] [ text message ]
            ]


viewStats : Statistics -> Element Msg
viewStats { excuseCount, exerciseCount, currentExerciseStreak, longestExerciseStreak, frequentExcuses, cloud } =
    let
        pieData : List ( Float, String )
        pieData =
            [ ( toFloat exerciseCount, "Exercises -" )
            , ( toFloat excuseCount, "Excuses -" )
            ]

        pieColors : List String
        pieColors =
            List.map Util.elementColorToString
                [ Colors.teal
                , Colors.red
                ]

        barData : List ( Float, String )
        barData =
            List.map (\( token, count ) -> ( toFloat count, token )) frequentExcuses
    in
    column
        [ spacing 14
        , paddingXY 20 0
        , width fill
        ]
        [ paragraph [ Region.heading 2, Font.size 24 ] [ text "All time statistics" ]
        , paragraph [] [ text <| "Current exercise streak: " ++ String.fromInt currentExerciseStreak ]
        , paragraph [] [ text <| "Longest exercise streak: " ++ String.fromInt longestExerciseStreak ]
        , Chart.pie pieData
            |> Chart.colors pieColors
            |> Chart.addValueToLabel
            |> Chart.updateStyles "container" [ ( "background-color", "white" ) ]
            |> Chart.toHtml
            |> Element.html
        , paragraph [ Region.heading 3, Font.size 24, Font.color Colors.red ] [ text "Your Excuses" ]
        , wordCloud cloud
        ]


eventToDate : Event -> Date
eventToDate { year, month, day } =
    Date.fromCalendarDate
        year
        (Date.numberToMonth month)
        day



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
        widget =
            initialWidgetModel flags

        pickerDateText =
            case widget of
                NewEventModel event ->
                    Date.format "yyyy-MM-dd" <| eventToDate event

                EditEventModel event ->
                    Date.format "yyyy-MM-dd" <| eventToDate event

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
      , isEventSaving = False
      , isEventDeleting = False
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

        AboutWidget ->
            AboutModel

        StatsWidget stats ->
            StatsModel stats

        FlashMessageWidget flashMessage ->
            FlashMessageModel flashMessage

        LoginWidget ->
            LoginModel initLoginModel

        NewUserWidget ->
            NewUserModel initNewUserModel


firstCharToUpper : String -> String
firstCharToUpper s =
    (s |> String.left 1 |> String.toUpper)
        ++ (s |> String.dropLeft 1)
