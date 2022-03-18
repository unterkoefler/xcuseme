{-# language DeriveAnyClass #-}

module Web.JsonTypes where

import Generated.Types
import IHP.Prelude
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm
import Application.Lib.DerivingViaElm ( ElmType(..) )
import Data.Time
import IHP.ModelSupport ( Violation(..) )
import Data.Char(toLower)

-- JSON serializable types and functions
-- -- for exposing IHP data to Elm and JSON responses

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

