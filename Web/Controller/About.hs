module Web.Controller.About where

import Web.Controller.Prelude
import Web.View.About.Index

instance Controller AboutController where
    action AboutAction = do
        render IndexView

