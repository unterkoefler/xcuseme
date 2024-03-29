{-# language DeriveAnyClass #-}

module Web.JsonTypes where

import Web.Types
import Generated.Types
import IHP.Prelude
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm
import Application.Lib.DerivingViaElm ( ElmType(..) )
import Data.Time
import IHP.ModelSupport ( Violation(..) )
import Data.Char (toLower)
import Application.WordCloud

-- JSON serializable types and functions
-- -- for exposing IHP data to Elm and JSON responses

deriving instance Generic Datum
deriving instance SOP.Generic Datum
deriving instance SOP.HasDatatypeInfo Datum
deriving instance Aeson.ToJSON Datum
deriving instance Aeson.FromJSON Datum

instance HasElmType Datum where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Datum defaultOptions "Api.Generated.Datum"

instance HasElmDecoder Aeson.Value Datum where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Datum defaultOptions Aeson.defaultOptions "Api.Generated.datumDecoder"

instance HasElmEncoder Aeson.Value Datum where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Datum defaultOptions Aeson.defaultOptions "Api.Generated.datumEncoder"

deriving instance Generic Position
deriving instance SOP.Generic Position
deriving instance SOP.HasDatatypeInfo Position
deriving instance Aeson.ToJSON Position
deriving instance Aeson.FromJSON Position

instance HasElmType Position where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Position defaultOptions "Api.Generated.Position"

instance HasElmDecoder Aeson.Value Position where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Position defaultOptions Aeson.defaultOptions "Api.Generated.positionDecoder"

instance HasElmEncoder Aeson.Value Position where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Position defaultOptions Aeson.defaultOptions "Api.Generated.positionEncoder"


deriving instance Generic PlacedWord
deriving instance SOP.Generic PlacedWord
deriving instance SOP.HasDatatypeInfo PlacedWord
deriving instance Aeson.ToJSON PlacedWord
deriving instance Aeson.FromJSON PlacedWord

instance HasElmType PlacedWord where
    elmDefinition =
        Just $ deriveElmTypeDefinition @PlacedWord defaultOptions "Api.Generated.PlacedWord"

instance HasElmDecoder Aeson.Value PlacedWord where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @PlacedWord defaultOptions Aeson.defaultOptions "Api.Generated.placedWordDecoder"

instance HasElmEncoder Aeson.Value PlacedWord where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @PlacedWord defaultOptions Aeson.defaultOptions "Api.Generated.placedWordEncoder"


deriving instance Generic Statistics
deriving instance SOP.Generic Statistics
deriving instance SOP.HasDatatypeInfo Statistics
deriving instance Aeson.ToJSON Statistics
deriving instance Aeson.FromJSON Statistics

instance HasElmType Statistics where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Statistics defaultOptions "Api.Generated.Statistics"

instance HasElmDecoder Aeson.Value Statistics where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Statistics defaultOptions Aeson.defaultOptions "Api.Generated.statisticsController"

instance HasElmEncoder Aeson.Value Statistics where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Statistics defaultOptions Aeson.defaultOptions "Api.Generated.statisticsEncoder"


deriving instance Generic EventType
deriving instance SOP.Generic EventType
deriving instance SOP.HasDatatypeInfo EventType
deriving instance Aeson.ToJSON EventType
deriving instance Aeson.FromJSON EventType

instance HasElmType EventType where
    elmDefinition =
        Just $ deriveElmTypeDefinition @EventType defaultOptions "Api.Generated.EventType"

instance HasElmDecoder Aeson.Value EventType where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @EventType defaultOptions Aeson.defaultOptions "Api.Generated.eventTypeDecoder"

instance HasElmEncoder Aeson.Value EventType where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @EventType defaultOptions Aeson.defaultOptions { Aeson.constructorTagModifier = map Data.Char.toLower } "Api.Generated.eventTypeEncoder"

deriving instance Generic Violation
deriving instance SOP.Generic Violation
deriving instance SOP.HasDatatypeInfo Violation
deriving instance Aeson.ToJSON Violation
deriving instance Aeson.FromJSON Violation

instance HasElmType Violation where
    elmDefinition =
        Just $ deriveElmTypeDefinition @Violation defaultOptions "Api.Generated.Violation"

instance HasElmDecoder Aeson.Value Violation where
    elmDecoderDefinition =
        Just $ deriveElmJSONDecoder @Violation defaultOptions Aeson.defaultOptions "Api.Generated.violationDecoder"

instance HasElmEncoder Aeson.Value Violation where
    elmEncoderDefinition =
        Just $ deriveElmJSONEncoder @Violation defaultOptions Aeson.defaultOptions "Api.Generated.violationEncoder"

data EventJSON = EventJSON
    { id :: Text
    , eventType :: EventType
    , description :: Text
    , day :: Int
    , month :: Int
    , year :: Int
    , errors :: ![(Text, Violation)]
    } deriving ( Generic
               , SOP.Generic
               , SOP.HasDatatypeInfo
               )
     deriving ( Aeson.ToJSON
              , Aeson.FromJSON
              , HasElmType
              , HasElmDecoder Aeson.Value
              , HasElmEncoder Aeson.Value)
        via ElmType "Api.Generated.Event" EventJSON

eventToJSON :: Event -> EventJSON
eventToJSON event =
    let date = get #date event
        (y, m, d) = toGregorian date
        metaBag = get #meta event
    in EventJSON {
        id = show $ get #id event,
        eventType = get #eventType event,
        description = get #description event,
        year = fromInteger y,
        month = m,
        day = d,
        errors = annotations metaBag
    }

data UserJSON = UserJSON
    { errors :: ![(Text, Violation)]
    } deriving ( Generic
               , SOP.Generic
               , SOP.HasDatatypeInfo
               )
     deriving ( Aeson.ToJSON
              , Aeson.FromJSON
              , HasElmType
              , HasElmDecoder Aeson.Value
              , HasElmEncoder Aeson.Value)
        via ElmType "Api.Generated.User" UserJSON


userToJSON :: User -> UserJSON
userToJSON user =
    let metaBag = get #meta user
    in UserJSON {
        errors = map fixFieldName $ annotations metaBag
    }

fixFieldName :: (Text, Violation) -> (Text, Violation)
fixFieldName ("passwordHash", violation) = ("password", violation)
fixFieldName other = other
