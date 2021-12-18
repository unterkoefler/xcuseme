{-# language DeriveAnyClass #-}

module Application.Helper.View (
    eventWidget,
    eventListWidget,
    eventCalendarWidget,
    Widget(..),
) where

-- Here you can add functions which are available in all your views
import IHP.ViewPrelude
import Generated.Types
import Data.Aeson as Aeson
import Web.JsonTypes
import qualified Generics.SOP as SOP
import GHC.Generics
import Language.Haskell.To.Elm

data Widget
    = EventWidget EventJSON
    | EventListWidget [EventJSON]
    | EventCalendarWidget [EventJSON]
    deriving ( Generic
             , Aeson.ToJSON
             , SOP.Generic
             , SOP.HasDatatypeInfo
             )

-- haskell-to-elm instances for the Widget type

instance HasElmType Widget where
  elmDefinition =
    Just $ "Api.Generated.Widget"
              |> deriveElmTypeDefinition @Widget
                Language.Haskell.To.Elm.defaultOptions

instance HasElmDecoder Aeson.Value Widget where
  elmDecoderDefinition =
    Just $ "Api.Generated.widgetDecoder"
              |> deriveElmJSONDecoder @Widget
                Language.Haskell.To.Elm.defaultOptions Aeson.defaultOptions

instance HasElmEncoder Aeson.Value Widget where
  elmEncoderDefinition =
    Just $ "Api.Generated.widgetEncoder"
              |> deriveElmJSONEncoder @Widget
                Language.Haskell.To.Elm.defaultOptions Aeson.defaultOptions


-- Widgets

eventWidget :: Event -> Html
eventWidget event = [hsx|
    <div data-flags={encode eventData} class="elm"></div>
|]
    where
        eventData :: Widget = EventWidget $ eventToJSON event

eventListWidget :: [Event] -> Html
eventListWidget events = [hsx|
    <div data-flags={encode eventListData} class="elm"></div>
|]
    where eventListData :: Widget = EventListWidget $ map eventToJSON events

eventCalendarWidget :: [Event] -> Html
eventCalendarWidget events = [hsx|
    <div data-flags={encode eventCalendarData} class="elm"></div>
|]
    where eventCalendarData :: Widget = EventCalendarWidget $ map eventToJSON events
