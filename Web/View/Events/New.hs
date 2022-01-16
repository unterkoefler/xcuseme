module Web.View.Events.New where
import Web.View.Prelude

data NewView = NewView { event :: Event }

instance View NewView where
    html NewView { .. } = [hsx|
        {renderForm event}
        {newEventWidget event}
    |]

renderForm :: Event -> Html
renderForm event = formFor event [hsx|
    {(textField #eventType)}
    {(textField #description)}
    {(dateField #date)}
    {submitButton}

|]
