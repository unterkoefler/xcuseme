module Web.View.User.New where

import Web.View.Prelude
import Web.JsonTypes

data NewView = NewView { user :: User }

instance View NewView where
    html NewView { .. } = newUserWidget

    json NewView { .. } = toJSON $ userToJSON user


