module Web.View.Events.New where
import Web.View.Prelude
import Data.Aeson
import Web.JsonTypes

data NewView = NewView { event :: Event }

instance View NewView where
    html NewView { .. } = [hsx|
        {newEventWidget event}
    |]

    json NewView { .. } = toJSON $ eventToJSON event

