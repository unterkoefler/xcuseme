module Web.View.Layout (defaultLayout, Html) where

import IHP.ViewPrelude
import IHP.Environment
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Generated.Types
import IHP.Controller.RequestContext
import IHP.FlashMessages.Types
import Web.Types
import Web.Routes
import Application.Helper.View

defaultLayout :: Html -> Html
defaultLayout inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    {icons}
    {metaTags}

    {stylesheets}
    {scripts}

    <title>{pageTitleOrDefault "XcuseMe"}</title>
</head>
<body>
    <div class="navBar">{navBarWidgetWithContext}</div>
    {myRenderFlashMessages}
    {inner}
</body>
|]

-- The 'assetPath' function used below appends a `?v=SOME_VERSION` to the static assets in production
-- This is useful to avoid users having old CSS and JS files in their browser cache once a new version is deployed
-- See https://ihp.digitallyinduced.com/Guide/assets.html for more details
--

navBarWidgetWithContext :: Html
navBarWidgetWithContext =
    case currentUserOrNothing of
        Nothing -> navBarWidget $ NavBarContext { loggedIn = False }
        Just _ -> navBarWidget $ NavBarContext { loggedIn = True }


stylesheets :: Html
stylesheets = [hsx|
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]

scripts :: Html
scripts = [hsx|
        {when isDevelopment devScripts}
        <script src={assetPath "/elm/index.js"}></script>
    |]

devScripts :: Html
devScripts = [hsx|
        <script id="livereload-script" src={assetPath "/livereload.js"} data-ws={liveReloadWebsocketUrl}></script>
    |]

metaTags :: Html
metaTags = [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta property="og:title" content="App"/>
    <meta property="og:type" content="website"/>
    <meta property="og:url" content="TODO"/>
    <meta property="og:description" content="TODO"/>
    {autoRefreshMeta}
|]

icons :: Html
icons = [hsx|
    <link rel="apple-touch-icon" sizes="180x180" href="/apple-touch-icon.png">
    <link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png">
    <link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png">
    <link rel="manifest" href="/site.webmanifest">
|]

myRenderFlashMessages :: (?context :: ControllerContext) => Html
myRenderFlashMessages = [hsx|
        <div>
            {forEach flashMessages flashMessageWidget}
        </div>
    |]
    where
        flashMessages :: [FlashMessage]
        flashMessages = fromFrozenContext
