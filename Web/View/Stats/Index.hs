module Web.View.Stats.Index where
import Web.View.Prelude

data IndexView = IndexView {
    stats :: Statistics
}

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div>{statsWidget stats}</div>
    |]
