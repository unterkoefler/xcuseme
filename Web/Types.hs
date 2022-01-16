module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import IHP.LoginSupport.Types
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

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
