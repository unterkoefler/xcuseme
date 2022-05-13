module Web.View.Events.List where
import Web.View.Prelude

data ListView = ListView { events :: [ Event ]  }

instance View ListView where
    html ListView { .. } = eventListWidget events
