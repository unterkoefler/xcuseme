module Web.Controller.User where

import Web.Controller.Prelude
import Web.View.User.New
import IHP.Prelude
import IHP.LoginSupport.Helper.Controller

instance Controller UserController where
   action CreateUserAction = do
        let password = maybe "" IHP.Prelude.id $ paramOrNothing @Text "password"
        let user = newRecord @User
        user
            |> fill @'["email"]
            |> set #passwordHash password
            |> validateField #email isEmail
            |> validateField #passwordHash nonEmpty
            |> validateField #passwordHash (hasMinLength 8)
            |> validateIsUniqueCaseInsensitive #email
            >>= ifValid \case
                Left user -> render NewView { .. }
                Right user -> do
                    hashed <- hashPassword (get #passwordHash user)
                    user <- user
                        |> set #passwordHash hashed
                        |> createRecord
                    setSuccessMessage "Welcome to XcuseMe! To get started, press one of the buttons below."
                    login user
                    render NewView { .. }


   action NewUserAction = do
       let user = newRecord
       render NewView { .. }
