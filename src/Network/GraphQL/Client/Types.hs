{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards #-}

module Network.GraphQL.Client.Types
  ( GraphQLQuery(..)
  , Location(..)
  , GraphQLError(..)
  , GraphQLQueryError(..)
  , GraphQLResponse(..)
  , GraphQLBody(..)
  , Nodes(..)
  )
where

import qualified Data.Aeson                    as J
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , (.=)
                                                )
import           Data.Void                      ( Void )
import           Data.Text                      ( Text )
import           Data.String                    ( IsString )
import           GHC.Generics

-----------------------------------------------------------------------------
-- | GraphQLQuery
-----------------------------------------------------------------------------
-- | The graphql query
newtype GraphQLQuery = GraphQLQuery { unGraphQLQuery :: Text }
  deriving (Show, Eq, J.ToJSON, J.FromJSON, IsString, Generic)

-----------------------------------------------------------------------------
-- | GraphQLQueryError
-----------------------------------------------------------------------------
data GraphQLQueryError = EmptyGraphQLReponse GraphQLQuery 
                       | HttpError Text
                       | GraphQLErrors [GraphQLError]
                       | ParsingError Text
  deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
-- | GraphQLError
-----------------------------------------------------------------------------
-- | GraphQL error response
data GraphQLError =
  GraphQLError { message :: Text
               , locations :: [Location]
               , path :: [Text]
               }
  deriving (Show, Eq, Generic)
instance J.ToJSON GraphQLError where
  toJSON = J.genericToJSON J.defaultOptions
instance J.FromJSON GraphQLError

-----------------------------------------------------------------------------
-- | Location
-----------------------------------------------------------------------------
-- | Location of the error in the GraphQL query
data Location =
  Location { line :: Integer
           , column :: Integer
           }
  deriving (Show, Eq, Generic)
instance J.ToJSON Location where
  toJSON = J.genericToJSON J.defaultOptions
instance J.FromJSON Location


-----------------------------------------------------------------------------
-- | GraphQLResponse
-----------------------------------------------------------------------------
-- | The response to the GraphQL query.
data GraphQLResponse a =
  GraphQLResponse { _data :: Maybe a
                  , errors :: Maybe [GraphQLError]
                  }
  deriving (Eq, Show, Generic)
instance (J.ToJSON a) => J.ToJSON (GraphQLResponse a) where
  toJSON GraphQLResponse {..} =
    J.object ["data" .= _data, "errors" .= errors]
instance (J.FromJSON a) => J.FromJSON (GraphQLResponse a) where
  parseJSON = J.withObject "GraphQLResponse"
    $ \v -> GraphQLResponse <$> v .:? "data" <*> v .:? "errors"

-----------------------------------------------------------------------------
-- | Nodes
-----------------------------------------------------------------------------
-- | Utility type for common json patterns parsed from graphql repsonse data.
-- e.g. `{ allPeople ::  Nodes Person }` 
data Nodes a = Nodes {nodes :: [a]}
  deriving (Eq, Show, Generic)
instance (J.ToJSON a) => J.ToJSON (Nodes a) where
  toJSON = J.genericToJSON J.defaultOptions
instance (J.FromJSON a) => J.FromJSON (Nodes a)

-----------------------------------------------------------------------------
-- | GraphQLBody
-----------------------------------------------------------------------------
-- | body object expected by GraphQL APIs.
data GraphQLBody a =
  GraphQLBody { query :: GraphQLQuery
              , variables :: Maybe a
              }
  deriving (Eq, Show, Generic)
instance (J.ToJSON a) => J.ToJSON (GraphQLBody a) where
  toJSON = J.genericToJSON J.defaultOptions
instance (J.FromJSON a) => J.FromJSON (GraphQLBody a)

