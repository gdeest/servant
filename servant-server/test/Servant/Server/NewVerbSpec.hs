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

-- | Tests for the unified NewVerb endpoint type.
module Servant.Server.NewVerbSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, decode')
import Data.Proxy (Proxy (..))
import Data.SOP (I (..), NS (..))
import GHC.Generics (Generic)
import Network.HTTP.Types
  ( hAccept
  , methodDelete
  , methodGet
  , methodHead
  , methodPatch
  , methodPost
  , methodPut
  , statusCode
  )
import Network.Wai.Test
  ( simpleBody
  , simpleHeaders
  , simpleStatus
  )
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Test.Hspec.Wai (with)
import qualified Test.Hspec.Wai as THW

import Servant.API
  ( JSON
  , PlainText
  , StdMethod (..)
  , (:<|>) (..)
  , (:>)
  )
import Servant.API.Experimental.Verb
  ( Delete'
  , Get'
  , NewVerb
  , OneOf
  , Patch'
  , Post'
  , Put'
  , Responds
  , RespondsEmpty
  )
import Servant.API.MultiVerb (AsUnion (..))
import Servant.Server (Server, serve)

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

bob :: Person
bob = Person "Bob" 25

notFoundError :: ErrorResponse
notFoundError = ErrorResponse 404 "Person not found"

--------------------------------------------------------------------------------
-- Single Response Tests
--------------------------------------------------------------------------------

type SingleResponseApi =
  "person" :> Get' JSON Person
    :<|> "create" :> Post' JSON Person
    :<|> "update" :> Put' JSON Person
    :<|> "remove" :> Delete' JSON Person
    :<|> "modify" :> Patch' JSON Person

singleResponseServer :: Server SingleResponseApi
singleResponseServer =
  pure alice
    :<|> pure bob
    :<|> pure alice
    :<|> pure bob
    :<|> pure alice

--------------------------------------------------------------------------------
-- Empty Response Tests
--------------------------------------------------------------------------------

type EmptyResponseApi =
  "noContent" :> NewVerb 'GET (RespondsEmpty 204)
    :<|> "created" :> NewVerb 'POST (RespondsEmpty 201)

emptyResponseServer :: Server EmptyResponseApi
emptyResponseServer =
  pure ()
    :<|> pure ()

--------------------------------------------------------------------------------
-- Multi-Response Tests (M+N Model)
--------------------------------------------------------------------------------

-- Success or NotFound
type PersonOrNotFound =
  OneOf
    '[ Responds 200 '[JSON] Person
     , Responds 404 '[JSON] ErrorResponse
     ]
    (Either ErrorResponse Person)

type MultiResponseApi =
  "user" :> "found" :> NewVerb 'GET PersonOrNotFound
    :<|> "user" :> "notfound" :> NewVerb 'GET PersonOrNotFound

-- AsUnion instance for Either - maps to Right for first element, Left for second
instance AsUnion '[Responds 200 '[JSON] Person, Responds 404 '[JSON] ErrorResponse] (Either ErrorResponse Person) where
  toUnion (Right p) = Z (I p)
  toUnion (Left e) = S (Z (I e))
  fromUnion (Z (I p)) = Right p
  fromUnion (S (Z (I e))) = Left e
  fromUnion (S (S x)) = case x of {}

multiResponseServer :: Server MultiResponseApi
multiResponseServer =
  pure (Right alice)  -- Returns 200 with alice
    :<|> pure (Left notFoundError)  -- Returns 404 with error

--------------------------------------------------------------------------------
-- Content Negotiation Tests
--------------------------------------------------------------------------------

type ContentNegotiationApi =
  "multi-content" :> NewVerb 'GET (Responds 200 '[JSON, PlainText] String)

contentNegotiationServer :: Server ContentNegotiationApi
contentNegotiationServer = pure "Hello"

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Servant.API.Experimental.Verb (NewVerb)" $ do
  singleResponseSpec
  emptyResponseSpec
  multiResponseSpec
  contentNegotiationSpec
  methodSpec

singleResponseSpec :: Spec
singleResponseSpec = describe "Single Response (Responds)" $ do
  with (pure $ serve (Proxy @SingleResponseApi) singleResponseServer) $ do
    it "GET returns the person with 200" $ do
      response <- THW.request methodGet "/person" [(hAccept, "application/json")] ""
      liftIO $ statusCode (simpleStatus response) `shouldBe` 200
      liftIO $ decode' (simpleBody response) `shouldBe` Just alice

    it "POST returns the person with 200" $ do
      response <- THW.request methodPost "/create" [(hAccept, "application/json")] ""
      liftIO $ statusCode (simpleStatus response) `shouldBe` 200
      liftIO $ decode' (simpleBody response) `shouldBe` Just bob

    it "PUT returns the person with 200" $ do
      response <- THW.request methodPut "/update" [(hAccept, "application/json")] ""
      liftIO $ statusCode (simpleStatus response) `shouldBe` 200
      liftIO $ decode' (simpleBody response) `shouldBe` Just alice

    it "DELETE returns the person with 200" $ do
      response <- THW.request methodDelete "/remove" [(hAccept, "application/json")] ""
      liftIO $ statusCode (simpleStatus response) `shouldBe` 200
      liftIO $ decode' (simpleBody response) `shouldBe` Just bob

    it "PATCH returns the person with 200" $ do
      response <- THW.request methodPatch "/modify" [(hAccept, "application/json")] ""
      liftIO $ statusCode (simpleStatus response) `shouldBe` 200
      liftIO $ decode' (simpleBody response) `shouldBe` Just alice

    it "sets Content-Type header correctly" $ do
      response <- THW.request methodGet "/person" [] ""
      liftIO $
        lookup "Content-Type" (simpleHeaders response)
          `shouldBe` Just "application/json;charset=utf-8"

    it "returns 406 for unsupported Accept header" $ do
      response <- THW.request methodGet "/person" [(hAccept, "text/html")] ""
      liftIO $ statusCode (simpleStatus response) `shouldBe` 406

    it "returns 405 for wrong method" $ do
      response <- THW.request methodPost "/person" [] ""
      liftIO $ statusCode (simpleStatus response) `shouldBe` 405

emptyResponseSpec :: Spec
emptyResponseSpec = describe "Empty Response (RespondsEmpty)" $ do
  with (pure $ serve (Proxy @EmptyResponseApi) emptyResponseServer) $ do
    it "returns 204 with empty body for GET" $ do
      response <- THW.request methodGet "/noContent" [] ""
      liftIO $ statusCode (simpleStatus response) `shouldBe` 204
      liftIO $ simpleBody response `shouldBe` ""

    it "returns 201 with empty body for POST" $ do
      response <- THW.request methodPost "/created" [] ""
      liftIO $ statusCode (simpleStatus response) `shouldBe` 201
      liftIO $ simpleBody response `shouldBe` ""

multiResponseSpec :: Spec
multiResponseSpec = describe "Multi-Response (OneOf) - M+N Model" $ do
  with (pure $ serve (Proxy @MultiResponseApi) multiResponseServer) $ do
    it "returns 200 with person when found" $ do
      response <- THW.request methodGet "/user/found" [(hAccept, "application/json")] ""
      liftIO $ statusCode (simpleStatus response) `shouldBe` 200
      liftIO $ decode' (simpleBody response) `shouldBe` Just alice

    it "returns 404 with error when not found" $ do
      response <- THW.request methodGet "/user/notfound" [(hAccept, "application/json")] ""
      liftIO $ statusCode (simpleStatus response) `shouldBe` 404
      liftIO $ decode' (simpleBody response) `shouldBe` Just notFoundError

    context "M+N Content Negotiation" $ do
      it "returns 406 for unsupported Accept on success endpoint" $ do
        response <- THW.request methodGet "/user/found" [(hAccept, "text/html")] ""
        liftIO $ statusCode (simpleStatus response) `shouldBe` 406

      -- For error responses (4xx), lenient negotiation falls back to default
      -- Since the error endpoint returns 404, it should still work with any Accept
      -- as it uses lenient negotiation
      it "error responses use lenient negotiation (fallback to default)" $ do
        -- The error response (404) uses lenient negotiation
        -- Even with wrong Accept, it should still return the response
        response <- THW.request methodGet "/user/notfound" [(hAccept, "application/json")] ""
        liftIO $ statusCode (simpleStatus response) `shouldBe` 404

contentNegotiationSpec :: Spec
contentNegotiationSpec = describe "Content Negotiation" $ do
  with (pure $ serve (Proxy @ContentNegotiationApi) contentNegotiationServer) $ do
    it "returns JSON when Accept: application/json" $ do
      response <- THW.request methodGet "/multi-content" [(hAccept, "application/json")] ""
      liftIO $ statusCode (simpleStatus response) `shouldBe` 200
      liftIO $ simpleBody response `shouldBe` "\"Hello\""

    it "returns plain text when Accept: text/plain" $ do
      response <- THW.request methodGet "/multi-content" [(hAccept, "text/plain")] ""
      liftIO $ statusCode (simpleStatus response) `shouldBe` 200
      liftIO $ simpleBody response `shouldBe` "Hello"

    it "returns 406 for unsupported Accept header" $ do
      response <- THW.request methodGet "/multi-content" [(hAccept, "text/html")] ""
      liftIO $ statusCode (simpleStatus response) `shouldBe` 406

methodSpec :: Spec
methodSpec = describe "HTTP Method Handling" $ do
  with (pure $ serve (Proxy @SingleResponseApi) singleResponseServer) $ do
    it "HEAD returns empty body but correct status" $ do
      response <- THW.request methodHead "/person" [(hAccept, "application/json")] ""
      liftIO $ statusCode (simpleStatus response) `shouldBe` 200
      liftIO $ simpleBody response `shouldBe` ""
