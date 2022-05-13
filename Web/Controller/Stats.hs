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
        let longestExerciseStreak = longestExerciseStreakF events
        render IndexView { stats = Statistics { .. } }

numExcuses :: [Event] -> Int
numExcuses =
    length . filter (\e -> Generated.Types.eventType e == Excuse)

numExercises :: [Event] -> Int
numExercises =
    length . filterExercises

longestExerciseStreakF :: [Event] -> Int
longestExerciseStreakF events = longestStreak (filterExercises events) 0 0 Nothing

longestStreak :: [Event] -> Int -> Int -> Maybe Event -> Int
longestStreak [] maxStreak _ _ = maxStreak
longestStreak (head:tail) maxStreak currentStreak maybeLaterEvent =
    let
        newStreak = case fmap (\e -> diffDays (date e) (date head)) maybeLaterEvent of
            Just 1 ->
                1 + currentStreak
            _ ->
                1
    in
        longestStreak tail (max maxStreak newStreak) newStreak (Just head)

currentExerciseStreakF :: [Event] -> Int
currentExerciseStreakF [] = 0
currentExerciseStreakF events@(head:rest) =
    if Generated.Types.eventType head == Excuse then
        0
    else
        currentStreak . filterExercises $ events

currentStreak :: [Event] -> Int
currentStreak [] = 0
currentStreak [_] = 1
currentStreak (first:second:rest) =
    if diffDays (date first) (date second) == 1 then
        1 + currentStreak (second:rest)
    else
        1

filterExercises :: [Event] -> [Event]
filterExercises =
    filter (\e -> Generated.Types.eventType e == Exercise)
