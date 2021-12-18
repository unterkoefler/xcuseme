module Web.View.Events.Edit where
import Web.View.Prelude

data EditView = EditView { event :: Event }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Event</h1>
        {renderForm event}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Events" (EventsAction "cal")
                , breadcrumbText "Edit Event"
                ]

renderForm :: Event -> Html
renderForm event = formFor event [hsx|
    {(textField #eventType)}
    {(textField #description)}
    {(textField #date)}
    {(textField #userId)}
    {submitButton}

|]
