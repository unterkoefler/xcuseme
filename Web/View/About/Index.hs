module Web.View.About.Index where
import Web.View.Prelude

data IndexView = IndexView

instance View IndexView where
    html IndexView = aboutWidget
