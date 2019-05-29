{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Data.Aeson                    as J
import           Data.FileEmbed                 ( embedStringFile )
import           GHC.Generics
import           Data.Text
import           Data.Void 
import           Network.GraphQL.Client         ( runQuery )
import           Network.GraphQL.Client.Types         (  GraphQLBody(..), Nodes )

-- TODO :: Implement better spec. for now simple test that will crash if failed.
main :: IO ()
main = do
  getAllTokens >>= print
  getEventDetail >>= print


-----------------------------------------------------------------------------
-- | AllTokens Query
-----------------------------------------------------------------------------
getAllTokens :: IO AllTokensResponse
getAllTokens = runQuery "https://api.pixura.io/graphql" allTokensBody >>= \case
  Left  errs -> error (show errs)
  Right edr  -> pure edr

allTokensBody :: GraphQLBody Void
allTokensBody = GraphQLBody
  { query     = $(embedStringFile "test/graphql/allTokens.graphql")
  , variables = Nothing
  }
data AllTokensResponse =
  AllTokensResponse { allErc721Tokens :: Nodes Erc721Token
                    }
  deriving (Eq, Show, Generic)
instance J.ToJSON AllTokensResponse where
  toJSON = J.genericToJSON J.defaultOptions
instance J.FromJSON AllTokensResponse

data Erc721Token =
  Erc721Token { tokenId :: Integer
              , owner :: Text
              , metadata :: TokenMetadata
              }
  deriving (Eq, Show, Generic)
instance J.ToJSON Erc721Token where
  toJSON = J.genericToJSON J.defaultOptions
instance J.FromJSON Erc721Token

data TokenMetadata =
  TokenMetadata { name :: Text
                , description :: Text
                , imageUri :: Text
                }
  deriving (Eq, Show, Generic)
instance J.ToJSON TokenMetadata where
  toJSON = J.genericToJSON J.defaultOptions
instance J.FromJSON TokenMetadata



-----------------------------------------------------------------------------
-- | EventDetail Query
-----------------------------------------------------------------------------

getEventDetail :: IO EventDetailResponse
getEventDetail =
  runQuery "https://api.pixura.io/graphql" (eventDetailBody eid) >>= \case
    Left  errs -> error (show errs)
    Right edr  -> pure edr

eid :: Text
eid = "0003de9b2bffbaf67fcd5e8b6a2c5c6e1b884ed1e8b22fc11a5b03ca0520aae4-23"

eventDetailBody :: Text -> GraphQLBody EventDetailArgs
eventDetailBody eid' = GraphQLBody
  { query     = $(embedStringFile "test/graphql/eventDetail.graphql")
  , variables = Just $ EventDetailArgs eid'
  }

data EventDetailArgs = EventDetailArgs { id :: Text }
  deriving (Eq, Show, Generic)
instance J.ToJSON EventDetailArgs where
  toJSON = J.genericToJSON J.defaultOptions
instance J.FromJSON EventDetailArgs

data EventDetailResponse =
  EventDetailResponse { eventDetail :: Maybe EventDetail
                      }
  deriving (Eq, Show, Generic)
instance J.ToJSON EventDetailResponse where
  toJSON = J.genericToJSON J.defaultOptions
instance J.FromJSON EventDetailResponse

data EventDetail =
  EventDetail { blockNumber :: Integer
              , blockTimestamp :: Text
              , blockHash :: Text
              , transactionHash :: Text
              , transactionIndex :: Integer
              , logIndex :: Integer
              , id :: Text
              , contractAddress :: Text
              }
  deriving (Eq, Show, Generic)
instance J.ToJSON EventDetail where
  toJSON = J.genericToJSON J.defaultOptions
instance J.FromJSON EventDetail
