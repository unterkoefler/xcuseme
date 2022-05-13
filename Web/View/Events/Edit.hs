module Web.View.Events.Edit where
import Web.View.Prelude
import Data.Aeson
import Web.JsonTypes

data EditView = EditView { event :: Event }

instance View EditView where
    html EditView { .. } = editEventWidget event

    json EditView { .. } = toJSON $ eventToJSON event

