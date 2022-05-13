module Web.View.Events.Index where
import Web.View.Prelude

data IndexView = IndexView { events :: [ Event ]  }

instance View IndexView where
    html IndexView { .. } = eventCalendarWidget events
