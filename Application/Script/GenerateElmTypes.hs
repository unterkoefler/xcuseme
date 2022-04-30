#!/usr/bin/env run-script
module Application.Script.GenerateElmTypes where

import Application.Helper.View
import Application.Script.Prelude
import qualified Data.HashMap.Lazy as HashMap
import Data.Text.IO
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import Language.Haskell.To.Elm
import qualified System.Directory as Directory
import Web.JsonTypes

run :: Script
run = do
    Directory.createDirectoryIfMissing False "elm/Api"
    forEach (HashMap.toList modules) $ \(_moduleName, contents) ->
        writeFile "elm/Api/Generated.elm" (show contents)
  where
    definitions =
        Simplification.simplifyDefinition
            -- Add Elm types here
            <$>
                jsonDefinitions @Widget
                <> jsonDefinitions @EventJSON
                <> jsonDefinitions @EventType
                <> jsonDefinitions @NavBarContext
                <> jsonDefinitions @Violation
                <> jsonDefinitions @FlashMessage
                <> jsonDefinitions @UserJSON

    modules = Pretty.modules definitions
