{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Client tests for the unified NewVerb endpoint type.
module Servant.NewVerbSpec (spec) where

import Control.Arrow (left)
import Control.Concurrent (ThreadId)
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (..))
import Data.SOP (I (..), NS (..))
import GHC.Generics (Generic)
import Test.Hspec

import Servant.API
  ( JSON
  , PlainText
  , StdMethod (..)
  , (:<|>) (..)
  , (:>)
  )
import Servant.API.Experimental.Verb
  ( Get'
  , NewVerb
  , OneOf
  , Responds
  , RespondsEmpty
  )
import Servant.API.MultiVerb (AsUnion (..))
import Servant.Client (BaseUrl, ClientM, client)
import Servant.ClientTestUtils (endWaiApp, runClient, startWaiApp)
import Servant.Server (Server, serve)

type TestEnv = (ThreadId, BaseUrl)

--------------------------------------------------------------------------------
-- Test Types
--------------------------------------------------------------------------------

data Person = Person
  { name :: String
  , age :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

data ErrorResponse = ErrorResponse
  { errorCode :: Int
  , message :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON ErrorResponse
instance FromJSON ErrorResponse

alice :: Person
alice = Person "Alice" 30

notFoundError :: ErrorResponse
notFoundError = ErrorResponse 404 "Person not found"

--------------------------------------------------------------------------------
-- API Definition
--------------------------------------------------------------------------------

-- Single response endpoint
type SingleResponseApi =
  "person" :> Get' JSON Person

-- Empty response endpoint
type EmptyResponseApi =
  "noContent" :> NewVerb 'GET (RespondsEmpty 204)
    :<|> "created" :> NewVerb 'POST (RespondsEmpty 201)

-- Multi-response endpoint (OneOf)
type PersonOrNotFound =
  OneOf
    '[ Responds 200 '[JSON] Person
     , Responds 404 '[JSON] ErrorResponse
     ]
    (Either ErrorResponse Person)

type MultiResponseApi =
  "user" :> "found" :> NewVerb 'GET PersonOrNotFound
    :<|> "user" :> "notfound" :> NewVerb 'GET PersonOrNotFound

-- Content negotiation endpoint
type ContentNegotiationApi =
  "multi-content" :> NewVerb 'GET (Responds 200 '[JSON, PlainText] String)

-- Combined API for all tests
type NewVerbTestApi =
  "single" :> SingleResponseApi
    :<|> "empty" :> EmptyResponseApi
    :<|> "multi" :> MultiResponseApi
    :<|> "content" :> ContentNegotiationApi

--------------------------------------------------------------------------------
-- AsUnion Instance
--------------------------------------------------------------------------------

instance AsUnion '[Responds 200 '[JSON] Person, Responds 404 '[JSON] ErrorResponse] (Either ErrorResponse Person) where
  toUnion (Right p) = Z (I p)
  toUnion (Left e) = S (Z (I e))
  fromUnion (Z (I p)) = Right p
  fromUnion (S (Z (I e))) = Left e
  fromUnion (S (S x)) = case x of {}

--------------------------------------------------------------------------------
-- Server Implementation
--------------------------------------------------------------------------------

singleResponseServer :: Server SingleResponseApi
singleResponseServer = pure alice

emptyResponseServer :: Server EmptyResponseApi
emptyResponseServer =
  pure ()
    :<|> pure ()

multiResponseServer :: Server MultiResponseApi
multiResponseServer =
  pure (Right alice)  -- Returns 200 with alice
    :<|> pure (Left notFoundError)  -- Returns 404 with error

contentNegotiationServer :: Server ContentNegotiationApi
contentNegotiationServer = pure "Hello"

newVerbTestServer :: Server NewVerbTestApi
newVerbTestServer =
  singleResponseServer
    :<|> emptyResponseServer
    :<|> multiResponseServer
    :<|> contentNegotiationServer

--------------------------------------------------------------------------------
-- Client Functions
--------------------------------------------------------------------------------

getSinglePerson :: ClientM Person
getNoContent :: ClientM ()
getCreated :: ClientM ()
getUserFound :: ClientM (Either ErrorResponse Person)
getUserNotFound :: ClientM (Either ErrorResponse Person)
getMultiContent :: ClientM String

getSinglePerson
  :<|> (getNoContent :<|> getCreated)
  :<|> (getUserFound :<|> getUserNotFound)
  :<|> getMultiContent = client (Proxy @NewVerbTestApi)

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Servant.NewVerbSpec" $
  beforeAll (startWaiApp (serve (Proxy @NewVerbTestApi) newVerbTestServer)) $
    afterAll endWaiApp $ do
      singleResponseClientSpec
      emptyResponseClientSpec
      multiResponseClientSpec
      contentNegotiationClientSpec

singleResponseClientSpec :: SpecWith TestEnv
singleResponseClientSpec = describe "Single Response (Responds)" $ do
  it "client successfully parses single response" $ \(_, baseUrl) -> do
    result <- runClient getSinglePerson baseUrl
    left show result `shouldBe` Right alice

emptyResponseClientSpec :: SpecWith TestEnv
emptyResponseClientSpec = describe "Empty Response (RespondsEmpty)" $ do
  it "client handles 204 No Content" $ \(_, baseUrl) -> do
    result <- runClient getNoContent baseUrl
    left show result `shouldBe` Right ()

  it "client handles 201 Created with empty body" $ \(_, baseUrl) -> do
    result <- runClient getCreated baseUrl
    left show result `shouldBe` Right ()

multiResponseClientSpec :: SpecWith TestEnv
multiResponseClientSpec = describe "Multi-Response (OneOf)" $ do
  it "client correctly dispatches 200 response" $ \(_, baseUrl) -> do
    result <- runClient getUserFound baseUrl
    left show result `shouldBe` Right (Right alice)

  it "client correctly dispatches 404 response" $ \(_, baseUrl) -> do
    result <- runClient getUserNotFound baseUrl
    left show result `shouldBe` Right (Left notFoundError)

contentNegotiationClientSpec :: SpecWith TestEnv
contentNegotiationClientSpec = describe "Content Negotiation" $ do
  it "client parses JSON response" $ \(_, baseUrl) -> do
    result <- runClient getMultiContent baseUrl
    left show result `shouldBe` Right "Hello"
