module Web.Controller.Stats where

import Web.Controller.Prelude
import Web.View.Stats.Index
import qualified Generated.Types

instance Controller StatsController where
    action StatsAction = do
        events <- query @Event
            |> filterWhere (#userId, currentUserId)
            |> orderByDesc #date
            |> fetch
        let excuseCount = numExcuses events
        let exerciseCount = numExercises events
        let currentExerciseStreak = currentExerciseStreakF events
        render IndexView { .. }

numExcuses :: [Event] -> Int
numExcuses =
    length . filter (\e -> Generated.Types.eventType e == Excuse)

numExercises :: [Event] -> Int
numExercises =
    length . filter (\e -> Generated.Types.eventType e == Exercise)

currentExerciseStreakF :: [Event] -> Int
currentExerciseStreakF [] = 0
currentExerciseStreakF events@(head:rest) =
    if Generated.Types.eventType head == Excuse then
        0
    else
        let exercises = filter (\e -> Generated.Types.eventType e == Exercise) events
        in
        currentStreak exercises

currentStreak :: [Event] -> Int
currentStreak [] = 0
currentStreak [_] = 1
currentStreak (first:second:rest) =
    if diffDays (date first) (date second) == 1 then
        1 + currentStreak (second:rest)
    else
        1
