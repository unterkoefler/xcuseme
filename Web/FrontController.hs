module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import IHP.LoginSupport.Middleware
import Web.Controller.Sessions
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Events
import Web.Controller.Static
import Web.Controller.User

instance FrontController WebApplication where
    controllers =
        [ startPage EventsAction
        , parseRoute @SessionsController
        , parseRoute @UserController
        -- Generator Marker
        , parseRoute @EventsController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
        initAuthentication @User
