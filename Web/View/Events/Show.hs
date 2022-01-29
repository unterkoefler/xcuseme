module Web.View.Events.Show where
import Web.View.Prelude

data ShowView = ShowView { event :: Event }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <p>{eventWidget event}</p>

    |]
