module XCalendar exposing (CellColor, CellColorArgs, view)

import Calendar
import Colors
import Date exposing (Date)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Material.Icons.Navigation exposing (chevron_left, chevron_right)
import Util exposing (icon)


type alias CellColorArgs =
    { currentDate : Date
    , date : Date
    , selectedDate : Date
    , dayDisplay : String
    }


type alias CellColor =
    { backgroundColor : Maybe Color, fontColor : Color }


view :
    { currentDate : Date
    , selectedDate : Date
    , monthIndex : Int
    , useDarkMode : Bool
    , onUpdateMonthIndex : Int -> msg
    , onDateSelected : Date -> msg
    , cellColor : CellColorArgs -> CellColor
    }
    -> Element msg
view args =
    let
        { monthIndex, currentDate } =
            args

        viewingDate =
            Date.add Date.Months monthIndex currentDate

        weeks =
            Calendar.fromDate Nothing viewingDate
    in
    column
        [ width fill
        , spacing 6
        ]
    <|
        ([ calendarNavRow
            { monthIndex = monthIndex
            , viewingDate = viewingDate
            , onUpdateMonthIndex = args.onUpdateMonthIndex
            }
         , weekDayLabels
         ]
            ++ List.map (\calendarDates -> row [ width fill ] (List.map (showDay args) calendarDates)) weeks
        )


calendarNavRow : { monthIndex : Int, viewingDate : Date, onUpdateMonthIndex : Int -> msg } -> Element msg
calendarNavRow { monthIndex, viewingDate, onUpdateMonthIndex } =
    row
        [ width fill
        , Background.color Colors.indigo
        , Font.color Colors.white
        , paddingXY 6 6
        ]
        [ Input.button [ alignLeft ]
            { label = icon chevron_left Colors.black 24
            , onPress = Just << onUpdateMonthIndex <| monthIndex - 1
            }
        , el [ centerX, paddingEach { left = 48, right = 0, top = 0, bottom = 0 } ] <| text <| Date.format "MMMM y" <| viewingDate
        , Input.button [ alignRight, paddingEach { left = 0, right = 12, bottom = 0, top = 0 }, width (px 48), Font.size 12 ] { label = text "today", onPress = Just << onUpdateMonthIndex <| 0 }
        , Input.button [ alignRight ]
            { label = icon chevron_right Colors.black 24
            , onPress = Just << onUpdateMonthIndex <| monthIndex + 1
            }
        ]


weekDayLabels : Element msg
weekDayLabels =
    let
        weekDays =
            [ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" ]
    in
    row
        [ width fill, paddingEach { top = 12, bottom = 0, left = 0, right = 0 } ]
        (weekDays
            |> List.map
                (\lbl ->
                    calendarCell
                        { bgColor = Nothing
                        , shrinkHeight = True
                        , innerContent = el [ Font.size 12, Font.center, width fill ] <| text lbl
                        }
                )
        )


showDay :
    { a
        | currentDate : Date
        , selectedDate : Date
        , monthIndex : Int
        , useDarkMode : Bool
        , onDateSelected : Date -> msg
        , cellColor : CellColorArgs -> CellColor
    }
    -> Calendar.CalendarDate
    -> Element msg
showDay { currentDate, selectedDate, useDarkMode, onDateSelected, cellColor } { dayDisplay, date } =
    let
        isFutureDate =
            Date.compare currentDate date == LT

        onPress =
            if isFutureDate then
                Nothing

            else
                Just <| onDateSelected date

        { backgroundColor, fontColor } =
            cellColor
                { currentDate = currentDate
                , selectedDate = selectedDate
                , date = date
                , dayDisplay = dayDisplay
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


calendarCell : { bgColor : Maybe Color, shrinkHeight : Bool, innerContent : Element msg } -> Element msg
calendarCell { bgColor, shrinkHeight, innerContent } =
    -- TODO -> scale with screen size
    let
        fontSize =
            12

        bgSize =
            36

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
        ]
    <|
        el
            attrs
            innerContent
