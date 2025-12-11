{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}

-- | Streaming client tests for the unified NewVerb endpoint type.
module Servant.NewVerbStreamSpec (spec) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import qualified Network.HTTP.Client as C
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

import Servant.API
  ( JSON
  , NewlineFraming
  , SourceIO
  , (:>)
  )
import Servant.API.Experimental.Verb (StreamGet')
import Servant.Client.Streaming
  ( BaseUrl
  , ClientError
  , ClientM
  , client
  , mkClientEnv
  , withClientM
  )
import qualified Servant.ClientTestUtils as CT
import Servant.Server (Server, serve)
import Servant.Types.SourceT (runSourceT, source)

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

alice :: Person
alice = Person "Alice" 30

bob :: Person
bob = Person "Bob" 25

--------------------------------------------------------------------------------
-- Streaming API Definition
--------------------------------------------------------------------------------

-- Standalone streaming endpoint using NewVerb
type StreamingApi =
  "stream" :> StreamGet' NewlineFraming JSON Person

--------------------------------------------------------------------------------
-- Server Implementation
--------------------------------------------------------------------------------

streamingServer :: Server StreamingApi
streamingServer = pure $ source [alice, bob, alice]

--------------------------------------------------------------------------------
-- Client Functions
--------------------------------------------------------------------------------

getStream :: ClientM (SourceIO Person)
getStream = client (Proxy @StreamingApi)

--------------------------------------------------------------------------------
-- Test Utilities
--------------------------------------------------------------------------------

{-# NOINLINE manager' #-}
manager' :: C.Manager
manager' = unsafePerformIO $ C.newManager C.defaultManagerSettings

withClient :: ClientM a -> BaseUrl -> (Either ClientError a -> IO r) -> IO r
withClient x baseUrl' = withClientM x (mkClientEnv manager' baseUrl')

testRunSourceIO :: SourceIO a -> IO (Either String [a])
testRunSourceIO = runExceptT . runSourceT

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Servant.NewVerbStreamSpec" $
  beforeAll (CT.startWaiApp (serve (Proxy @StreamingApi) streamingServer)) $
    afterAll CT.endWaiApp $ do
      streamingClientSpec

streamingClientSpec :: SpecWith (a, BaseUrl)
streamingClientSpec = describe "Streaming Response (RespondsStream)" $ do
  it "client consumes JSON Lines stream" $ \(_, baseUrl) -> do
    withClient getStream baseUrl $ \(Right res) ->
      testRunSourceIO res `shouldReturn` Right [alice, bob, alice]
