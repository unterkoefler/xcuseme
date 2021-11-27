module Web.Controller.User where

import Web.Controller.Prelude
import Web.View.User.New

instance Controller UserController where
   action CreateUserAction = do
        let user = newRecord @User
        user
            |> fill @["email", "passwordHash"]
            |> validateField #email isEmail
            |> validateField #passwordHash nonEmpty
            |> validateIsUniqueCaseInsensitive #email
            >>= ifValid \case
                Left user -> render NewView { .. }
                Right user -> do
                    hashed <- hashPassword (get #passwordHash user)
                    user <- user
                        |> set #passwordHash hashed
                        |> createRecord
                    setSuccessMessage "You have registered successfully"
                    redirectToPath "/"


   action NewUserAction = do
       let user = newRecord
       render NewView { .. }
