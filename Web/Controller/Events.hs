module Web.Controller.Events where

import Web.Controller.Prelude
import Web.View.Events.Index
import Web.View.Events.New
import Web.View.Events.Edit
import Web.View.Events.Show
import Web.View.Events.List
import ValidationSupport.ValidateIsUniqueMultiColumn

instance Controller EventsController where
    beforeAction = ensureIsUser

    action (EventsAction mode) = do
        events <- query @Event
            |> filterWhere (#userId, currentUserId)
            |> fetch
        case toLower mode of
            "list" ->
                render ListView { .. }
            _ ->
                render IndexView { .. }

    action (NewEventAction eventType) = do
        let eventType' = do
            case toLower eventType of
                "exercise" -> Exercise
                "excuse" -> Excuse
                _ -> Exercise
        let event = newRecord |> set #eventType eventType'
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
                    redirectTo EditEventAction { .. }

    action CreateEventAction = do
        let event = newRecord @Event
        event
            |> buildEvent
            >>= ifValid \case
                Left event -> render NewView { .. }
                Right event -> do
                    event <- event |> createRecord
                    setSuccessMessage "Event created"
                    redirectTo (EventsAction "cal")

    action DeleteEventAction { eventId } = do
        event <- fetch eventId
        accessDeniedUnless (get #userId event == currentUserId)

        deleteRecord event
        setSuccessMessage "Event deleted"
        redirectTo (EventsAction "cal")

buildEvent event = event
    |> set #userId currentUserId
    |> fill @["eventType","description","date"]
    |> validateField #description nonEmpty
    |> validateIsUniqueTwoColumn #userId #date
