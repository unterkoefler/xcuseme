{-# language DeriveAnyClass #-}

module Application.Helper.View (
    eventWidget,
    eventListWidget,
    eventCalendarWidget,
    navBarWidget,
    newEventWidget,
    Widget(..),
    NavBarContext(..)
) where

-- Here you can add functions which are available in all your views
import IHP.ViewPrelude
import Generated.Types
import Data.Aeson as Aeson
import Web.JsonTypes
import qualified Generics.SOP as SOP
import GHC.Generics
import Language.Haskell.To.Elm
import Application.Lib.DerivingViaElm ( ElmType(..) )

data Widget
    = EventWidget EventJSON
    | EventListWidget [EventJSON]
    | EventCalendarWidget [EventJSON]
    | NavBarWidget NavBarContext
    | NewEventWidget EventJSON
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


data NavBarContext = NavBarContext
    { loggedIn :: Bool
    } deriving ( Generic
               , SOP.Generic
               , SOP.HasDatatypeInfo
               )
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value)
        via ElmType "Api.Generated.NavBarContext" NavBarContext

-- Widgets

eventWidget :: Event -> Html
eventWidget event = [hsx|
    <div data-flags={encode eventData} class="elm"></div>
|]
    where
        eventData :: Widget = EventWidget $ eventToJSON event

newEventWidget :: Event -> Html
newEventWidget event = [hsx|
    <div data-flags={encode eventData} class="elm"></div>
|]
    where
        eventData :: Widget = NewEventWidget $ eventToJSON event

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

navBarWidget :: NavBarContext -> Html
navBarWidget context = [hsx|
    <div data-flags={encode $ NavBarWidget context} class="elm"></div>
|]
