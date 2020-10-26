{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Network.GraphQL.Client
  ( runQuery
  , W.Auth(..)
  , tlsManagerSettings
  , newManager
  , Manager(..)
  )
where

import           Control.Monad.IO.Class         ( MonadIO(..) )
import qualified Data.Aeson                    as J
import qualified Network.Wreq                  as W
import           Data.Text                      ( pack )
import           Control.Lens.Operators         ( (&)
                                                , (.~)
                                                , (^.)
                                                )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Network.HTTP.Client            ( newManager
                                                , Manager(..)
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
  => Maybe Manager
  -> String
  -> Maybe W.Auth
  -> GraphQLBody a
  -> m (Either GraphQLQueryError (GraphQLResponse b))
runQuery mmgr uri mauth body = do
  let optsNoAuth =
        maybe W.defaults (\m -> W.defaults & W.manager .~ Right m) mmgr
      opts = optsNoAuth & W.auth .~ mauth
  runQueryWithOptions opts uri body

-----------------------------------------------------------------------------
-- | runQueryWithOptions
-----------------------------------------------------------------------------
-- | Runs the provided GraphQLQuery on the uri returning either the result
--   or the error.
runQueryWithOptions
  :: (J.ToJSON a, J.FromJSON b, MonadIO m)
  => W.Options
  -> String
  -> GraphQLBody a
  -> m (Either GraphQLQueryError (GraphQLResponse b))
runQueryWithOptions opts uri body = do
  res <- liftIO $ W.postWith opts uri (J.toJSON body)
  pure $ case J.eitherDecode (res ^. W.responseBody) of
    Left  err -> Left $ ParsingError (pack err)
    Right (GraphQLResponse (Just gqlData) errs) -> case J.fromJSON gqlData of
      J.Error   msg -> Left $ ParsingError (pack msg)
      J.Success v   -> Right $ GraphQLResponse (Just v) errs
    Right (GraphQLResponse Nothing Nothing) -> Left EmptyGraphQLReponse
    Right (GraphQLResponse Nothing errs) ->
      Right $ GraphQLResponse Nothing errs
