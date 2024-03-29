module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import IHP.LoginSupport.Types
import Generated.Types
import Application.WordCloud

data WebApplication = WebApplication deriving (Eq, Show)


instance HasNewSessionUrl User where
    newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User

data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    deriving (Eq, Show, Data)

data UserController
    = NewUserAction
    | CreateUserAction
    deriving (Eq, Show, Data)

deriving instance Data EventType

data EventsController
    = EventsAction { mode :: Text }
    | NewEventAction { eventType :: Text, year :: Maybe Int, month :: Maybe Int, day :: Maybe Int }
    | ShowEventAction { eventId :: !(Id Event) }
    | CreateEventAction
    | EditEventAction { eventId :: !(Id Event) }
    | UpdateEventAction { eventId :: !(Id Event) }
    | DeleteEventAction { eventId :: !(Id Event) }
    deriving (Eq, Show, Data)

data AboutController
    = AboutAction
    deriving (Eq, Show, Data)

data StatsController
    = StatsAction
    deriving (Eq, Show, Data)

data Statistics = Statistics {
    excuseCount :: Int,
    exerciseCount :: Int,
    currentExerciseStreak :: Int,
    longestExerciseStreak :: Int,
    frequentExcuses :: [(Text, Int)],
    cloud :: [PlacedWord]
}
