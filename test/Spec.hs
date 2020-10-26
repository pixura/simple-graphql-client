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
import           Network.GraphQL.Client         
import           Network.GraphQL.Client.Types   

-- TODO :: Implement better spec. for now simple test that will crash if failed.
main :: IO ()
main = do
  testCountries >>= print


-----------------------------------------------------------------------------
-- | TestCountries Query
-----------------------------------------------------------------------------
testCountries :: IO (GraphQLResponse TestCountriesResponse)
testCountries =
  runQuery Nothing "https://countries.trevorblades.com/" Nothing testCountriesBody
    >>= \case
          Left  errs -> error (show errs)
          Right edr  -> pure edr

testCountriesBody :: GraphQLBody J.Value
testCountriesBody = GraphQLBody
  { graphQLBodyQuery     = $(embedStringFile "test/graphql/TestCountries.graphql")
  , graphQLBodyVariables = Just $ J.object ["code" J..= ("AF" :: String)]
  }
data TestCountriesResponse = TestCountriesResponse
  { continent :: Continent
  }
  deriving (Eq, Show, Generic)
instance J.ToJSON TestCountriesResponse where
  toJSON = J.genericToJSON J.defaultOptions
instance J.FromJSON TestCountriesResponse

data Continent = Continent
  { code  :: Text
  , name :: Text
  }
  deriving (Eq, Show, Generic)
instance J.ToJSON Continent where
  toJSON = J.genericToJSON J.defaultOptions
instance J.FromJSON Continent


