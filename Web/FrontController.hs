module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import IHP.LoginSupport.Middleware
import Web.Controller.Sessions
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.About
import Web.Controller.Events
import Web.Controller.Static
import Web.Controller.User
import Web.Controller.Stats

instance FrontController WebApplication where
    controllers =
        [ startPage (EventsAction "cal")
        , parseRoute @SessionsController
        , parseRoute @UserController
        , parseRoute @StatsController
        -- Generator Marker
        , parseRoute @AboutController
        , parseRoute @EventsController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
        initAuthentication @User
