{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import           Control.Monad.Catch            ( Exception(..) )
import           Data.Char                      ( toLower )
import           Data.Text                      ( Text )
import           Data.String                    ( IsString )
import           GHC.Generics

defaultJSONOptions :: String -> J.Options
defaultJSONOptions n = J.defaultOptions
  { J.fieldLabelModifier = lowerFirst . drop (length n)
  }
 where
  lowerFirst []       = []
  lowerFirst (x : xs) = (toLower x) : xs

-----------------------------------------------------------------------------
-- | GraphQLQuery
-----------------------------------------------------------------------------
-- | The graphql query
newtype GraphQLQuery = GraphQLQuery { unGraphQLQuery :: Text }
  deriving (Show, Eq, J.ToJSON, J.FromJSON, IsString, Generic)

-----------------------------------------------------------------------------
-- | GraphQLQueryError
-----------------------------------------------------------------------------
data GraphQLQueryError = EmptyGraphQLReponse
                       | HttpError Text
                       | ParsingError Text
  deriving (Show, Eq, Generic)

instance Exception GraphQLQueryError
-----------------------------------------------------------------------------
-- | GraphQLError
-----------------------------------------------------------------------------
-- | GraphQL error response
data GraphQLError = GraphQLError
  { graphQLErrorMessage   :: Text
  , graphQLErrorLocations :: Maybe [Location]
  , graphQLErrorPath      :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)
instance J.ToJSON GraphQLError where
  toJSON = J.genericToJSON $ defaultJSONOptions "GraphQLError"
instance J.FromJSON GraphQLError where
  parseJSON = J.genericParseJSON $ defaultJSONOptions "GraphQLError"

-----------------------------------------------------------------------------
-- | Location
-----------------------------------------------------------------------------
-- | Location of the error in the GraphQL query
data Location = Location
  { locationLine   :: Integer
  , locationColumn :: Integer
  }
  deriving (Show, Eq, Generic)
instance J.ToJSON Location where
  toJSON = J.genericToJSON $ defaultJSONOptions "Location"
instance J.FromJSON Location where
  parseJSON = J.genericParseJSON $ defaultJSONOptions "Location"


-----------------------------------------------------------------------------
-- | GraphQLResponse
-----------------------------------------------------------------------------
-- | The response to the GraphQL query.
data GraphQLResponse a = GraphQLResponse
  { graphQLResponseData   :: Maybe a
  , graphQLResponseErrors :: Maybe [GraphQLError]
  }
  deriving (Eq, Show, Generic)
instance J.ToJSON a => J.ToJSON (GraphQLResponse a)  where
  toJSON = J.genericToJSON $ defaultJSONOptions "GraphQLResponse"
instance J.FromJSON a => J.FromJSON (GraphQLResponse a) where
  parseJSON = J.genericParseJSON $ defaultJSONOptions "GraphQLResponse"

-----------------------------------------------------------------------------
-- | Nodes
-----------------------------------------------------------------------------
-- | Utility type for common json patterns parsed from graphql repsonse data.
-- e.g. `{ allPeople ::  Nodes Person }` 
data Nodes a = Nodes
  { nodes :: [a]
  }
  deriving (Eq, Show, Generic)
instance (J.ToJSON a) => J.ToJSON (Nodes a) where
  toJSON = J.genericToJSON J.defaultOptions
instance (J.FromJSON a) => J.FromJSON (Nodes a)

-----------------------------------------------------------------------------
-- | GraphQLBody
-----------------------------------------------------------------------------
-- | body object expected by GraphQL APIs.
data GraphQLBody a = GraphQLBody
  { graphQLBodyQuery     :: GraphQLQuery
  , graphQLBodyVariables :: Maybe a
  }
  deriving (Eq, Show, Generic)
instance (J.ToJSON a) => J.ToJSON (GraphQLBody a) where
  toJSON = J.genericToJSON $ defaultJSONOptions "GraphQLBody"
instance (J.FromJSON a) => J.FromJSON (GraphQLBody a) where
  parseJSON = J.genericParseJSON $ defaultJSONOptions "GraphQLBody"

