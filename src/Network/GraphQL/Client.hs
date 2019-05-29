{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Network.GraphQL.Client
  ( runQuery
  )
where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.Aeson                    as J
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , (.=)
                                                )
import qualified Network.Wreq                  as W
import           Data.Text                      ( pack )
import           Control.Lens.Operators         ( (&)
                                                , (.~)
                                                , (^.)
                                                )

import           Network.GraphQL.Client.Types   ( GraphQLBody(..)
                                                , GraphQLQueryError(..)
                                                , GraphQLResponse(..)
                                                )

-----------------------------------------------------------------------------
-- | runQuery
-----------------------------------------------------------------------------
-- | Runs the provided GraphQLQuery on the uri returning either the result
--   or the error.
runQuery
  :: (J.ToJSON a, J.FromJSON b, MonadIO m)
  => String
  -> GraphQLBody a
  -> m (Either GraphQLQueryError b)
runQuery uri body = do
  res <- liftIO $ W.post uri (J.toJSON body)
  pure $ case J.eitherDecode (res ^. W.responseBody) of
    Left  err -> Left $ ParsingError (pack err)
    Right (GraphQLResponse (Just gqlData) Nothing) -> Right gqlData
    Right (GraphQLResponse Nothing Nothing) ->
      Left $ EmptyGraphQLReponse (query body)
    Right (GraphQLResponse _ (Just errs)) -> Left $ GraphQLErrors errs

