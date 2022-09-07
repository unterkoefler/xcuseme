module Web.Controller.Events where

import Web.Controller.Prelude
import Web.View.Events.Index
import Web.View.Events.New
import Web.View.Events.Edit
import Web.View.Events.Show
import Web.View.Events.List
import ValidationSupport.ValidateIsUniqueMultiColumn
import Data.Time
import Data.Time.Clock
import Data.Aeson

instance Controller EventsController where
    beforeAction = ensureIsUser

    action (EventsAction mode) = do
        events <- query @Event
            |> filterWhere (#userId, currentUserId)
            |> orderByDesc #date
            |> fetch
        case toLower mode of
            "list" ->
                render ListView { .. }
            _ ->
                render IndexView { .. }

    action NewEventAction { eventType, year, month, day } = do
        let eventType' = eventTypeFromParam eventType
        date <- do
            case (year, month, day) of
                (Just y, Just m, Just d) ->
                    pure $ fromGregorian (toInteger y) m d
                _ ->
                    fmap utctDay getCurrentTime
        let event = newRecord |> set #eventType eventType'
                              |> set #date date
        render NewView { .. }

    action ShowEventAction { eventId } = do
        event <- fetch eventId
        accessDeniedUnless (get #userId event == currentUserId)
        render ShowView { .. }

    action EditEventAction { eventId } = do
        event <- fetch eventId
        accessDeniedUnless (get #userId event == currentUserId)
        render EditView { .. }

    action UpdateEventAction { eventId } = do
        event <- fetch eventId
        accessDeniedUnless (get #userId event == currentUserId)

        event
            |> buildEvent
            >>= ifValid \case
                Left event -> render EditView { .. }
                Right event -> do
                    event <- event |> updateRecord
                    setSuccessMessage "Event updated"
                    render EditView { .. }

    action CreateEventAction = do
        let event = newRecord @Event
        event
            |> buildEvent
            >>= ifValid \case
                Left event -> render NewView { .. }
                Right event -> do
                    event <- event |> createRecord
                    setSuccessMessage "Event created"
                    render NewView { .. }

    action DeleteEventAction { eventId } = do
        event <- fetch eventId
        accessDeniedUnless (get #userId event == currentUserId)

        deleteRecord event
        setSuccessMessage "Event deleted"
        renderJson (object [ "success" .= True ])

buildEvent event = event
    |> set #userId currentUserId
    |> fill @["eventType","description","date"]
    |> validateField #description nonEmpty
    |> validateIsUniqueTwoColumn #userId #date (Just "non_unique_date")

eventTypeFromParam :: Text -> EventType
eventTypeFromParam eventType =
    case toLower eventType of
        "exercise" ->
            Exercise
        "excuse" ->
            Excuse
        _ ->
            Exercise
