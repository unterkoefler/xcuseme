module RelativeDate exposing (toString)

import Date exposing (Date, Unit(..))


toString : { today : Date, other : Date } -> String
toString args =
    toStringHelp args ++ "."


toStringHelp : { today : Date, other : Date } -> String
toStringHelp { today, other } =
    let
        weekday =
            Date.format "EEEE" other

        daysAgo =
            Date.diff Days other today

        weeksAgo =
            Date.diff Weeks other today

        monthsAgo =
            Date.diff Months other today
    in
    if daysAgo < 0 then
        "That's in the future. Please pick another date."

    else if daysAgo == 0 then
        "That's today, a " ++ weekday

    else if daysAgo == 1 then
        "That was yesterday, a " ++ weekday

    else if daysAgo < 7 then
        "That was " ++ String.fromInt daysAgo ++ " days ago, a " ++ weekday

    else if daysAgo == 7 then
        "That was last " ++ weekday

    else if weeksAgo == 1 then
        "That was a " ++ weekday ++ ", " ++ String.fromInt weeksAgo ++ " week ago"

    else if weeksAgo < 5 && monthsAgo == 0 then
        "That was a " ++ weekday ++ ", " ++ String.fromInt weeksAgo ++ " weeks ago"

    else if monthsAgo == 1 then
        "That was a " ++ weekday ++ ", 1 month ago"

    else if monthsAgo < 12 then
        "That was a " ++ weekday ++ ", " ++ String.fromInt monthsAgo ++ " months ago"

    else
        "That was a " ++ weekday ++ " a long time ago"
