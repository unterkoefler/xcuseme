module ValidationSupport.ValidateIsUniqueMultiColumn
( validateIsUniqueTwoColumn
) where



import IHP.Prelude
import Database.PostgreSQL.Simple.ToField
import IHP.ModelSupport
import IHP.ValidationSupport.Types
import IHP.QueryBuilder
import IHP.Fetch

validateIsUniqueTwoColumn :: forall field1 field2 model savedModel field1Value field2Value modelId savedModelId. (
        savedModel ~ NormalizeModel model
        , ?modelContext :: ModelContext
        , FromRow savedModel
        , KnownSymbol field1
        , HasField field1 model field1Value
        , HasField field1 savedModel field1Value
        , KnownSymbol (GetTableName savedModel)
        , ToField field1Value
        , EqOrIsOperator field1Value
        , KnownSymbol field2
        , HasField field2 model field2Value
        , HasField field2 savedModel field2Value
        , ToField field2Value
        , EqOrIsOperator field2Value
        , HasField "meta" model MetaBag
        , SetField "meta" model MetaBag
        , HasField "id" savedModel savedModelId
        , HasField "id" model modelId
        , savedModelId ~ modelId
        , Eq modelId
        , Show modelId
        , GetModelByTableName (GetTableName savedModel) ~ savedModel
        , Table savedModel
    ) => Proxy field1 -> Proxy field2 -> Maybe Text -> model -> IO model
validateIsUniqueTwoColumn field1Proxy field2Proxy customError model = do
    let value1 = getField @field1 model
        value2 = getField @field2 model
        errorMessage = maybe "This is already in use" id customError
    result <- query @savedModel
            |> filterWhere (field1Proxy, value1)
            |> filterWhere (field2Proxy, value2)
            |> fetchOneOrNothing
    case result of
        Just value | not $ (getField @"id" model) == (getField @"id" value) -> do
            pure (attachValidatorResult field2Proxy (Failure (errorMessage ++ ":" ++ (show $ getField @"id" value))) model)
        _ -> pure (attachValidatorResult field2Proxy Success model)
