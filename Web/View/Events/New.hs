module Web.View.Events.New where
import Web.View.Prelude

data NewView = NewView { event :: Event }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Event</h1>
        {renderForm event}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Events" (EventsAction "cal")
                , breadcrumbText "New Event"
                ]

renderForm :: Event -> Html
renderForm event = formFor event [hsx|
    {(textField #eventType)}
    {(textField #description)}
    {(dateField #date)}
    {submitButton}

|]
