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
        Just $ deriveElmJSONEncoder @EventType defaultOptions Aeson.defaultOptions "Api.Generated.eventTypeEncoder"


data EventJSON = EventJSON
    { id :: Text
    , eventType :: EventType
    , description :: Text
    , day :: Int
    , month :: Int
    , year :: Int
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
    in EventJSON {
        id = show $ get #id event,
        eventType = get #eventType event,
        description = get #description event,
        year = fromInteger y,
        month = m,
        day = d
    }

