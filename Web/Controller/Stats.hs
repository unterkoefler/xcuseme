module Web.Controller.Stats where

import Web.Controller.Prelude
import Web.View.Stats.Index
import qualified Generated.Types
import NLP.Tokenize.Text
import qualified Data.Set

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
        let frequentExcuses = filter (\(w, count) -> count > 1) . sortBy sorter . counts . filter (\w -> notElem w boringWords) . excuseWords $ events
        render IndexView { stats = Statistics { .. } }

sorter :: (Text, Int) -> (Text, Int) -> Ordering
sorter count1 count2 =
    compare (snd count2) (snd count1)

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


filterExcuses :: [Event] -> [Event]
filterExcuses =
    filter (\e -> Generated.Types.eventType e == Excuse)


counts :: [Text] -> [(Text, Int)]
counts [] = []
counts (x:xs) = (x, succ . length . filter (== x) $ xs):counts (filter (/= x) xs)

excuseWords :: [Event] -> [Text]
excuseWords events =
    let blob :: Text
        blob =
            foldl (\acc event -> acc ++ description event ++ " ") "" (filterExcuses events)


    in
        NLP.Tokenize.Text.run myTokenizer $ toLower blob

boringWords :: Set Text
boringWords =
    Data.Set.fromList
        [ "and"
        , "in"
        , "."
        , "on"
        , "then"
        , "the"
        , "was"
        , "it"
        , "went"
        , "from"
        , "to"
        , "a"
        , "not"
        , "i"
        , "for"
        , "at"
        , "but"
        , "too"
        , "also"
        , "got"
        , "am"
        , "an"
        , "really"
        , "of"
        , "very"
        , "with"
        , "lot"
        , "my"
        , "("
        , ")"
        , "had"
        , "going"

        , "w"
        , ","
        , ":"
        , "day"
        ]

myTokenizer :: Tokenizer
myTokenizer =     whitespace
              >=> uris
              >=> punctuation
              >=> contractions
