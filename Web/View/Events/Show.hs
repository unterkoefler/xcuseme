module Web.View.Events.Show where
import Web.View.Prelude

data ShowView = ShowView { event :: Event }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Event</h1>
        <p>{eventWidget event}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Events" (EventsAction "cal")
                            , breadcrumbText "Show Event"
                            ]
