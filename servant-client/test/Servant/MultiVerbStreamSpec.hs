{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -freduction-depth=100 #-}

-- | Tests for MultiVerb streaming support with RespondStream.
module Servant.MultiVerbStreamSpec (spec) where

import Control.Monad.Trans.Except
import Data.Proxy
import qualified Network.HTTP.Client as C
import Prelude.Compat
import Servant.API
  ( JSON
  , NetstringFraming
  , NewlineFraming
  , SourceIO
  , StdMethod (GET)
  , (:<|>) ((:<|>))
  , (:>)
  )
import Servant.API.MultiVerb
import Servant.Server
import Servant.Types.SourceT
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Prelude ()

import Servant.Client.Streaming
import Servant.ClientTestUtils (Person (..), endWaiApp, startWaiApp)

-- | MultiVerb API with streaming responses using RespondStream.
-- This tests that MultiVerb can properly dispatch streaming requests
-- to use withStreamingRequest.
type MultiVerbStreamApi =
  "streamNewline"
    :> MultiVerb
         'GET
         '[JSON]
         '[RespondStream 200 "OK" NewlineFraming JSON Person]
         (SourceIO Person)
    :<|> "streamNetstring"
      :> MultiVerb
           'GET
           '[JSON]
           '[RespondStream 200 "OK" NetstringFraming JSON Person]
           (SourceIO Person)

api :: Proxy MultiVerbStreamApi
api = Proxy

getNewline :: ClientM (SourceIO Person)
getNetstring :: ClientM (SourceIO Person)
getNewline :<|> getNetstring = client api

alice :: Person
alice = Person "Alice" 42

bob :: Person
bob = Person "Bob" 25

server :: Application
server =
  serve api $
    pure (source [alice, bob, alice])
      :<|> pure (source [alice, bob, alice])

{-# NOINLINE manager' #-}
manager' :: C.Manager
manager' = unsafePerformIO $ C.newManager C.defaultManagerSettings

withClient :: ClientM a -> BaseUrl -> (Either ClientError a -> IO r) -> IO r
withClient x baseUrl' = withClientM x (mkClientEnv manager' baseUrl')

testRunSourceIO
  :: SourceIO a
  -> IO (Either String [a])
testRunSourceIO = runExceptT . runSourceT

spec :: Spec
spec = describe "Servant.Client.MultiVerb.Streaming" $ do
  multiVerbStreamSpec

multiVerbStreamSpec :: Spec
multiVerbStreamSpec = beforeAll (startWaiApp server) $ afterAll endWaiApp $ do
  it "streams with NewlineFraming via MultiVerb" $ \(_, baseUrl) -> do
    withClient getNewline baseUrl $ \(Right res) ->
      testRunSourceIO res `shouldReturn` Right [alice, bob, alice]

  it "streams with NetstringFraming via MultiVerb" $ \(_, baseUrl) -> do
    withClient getNetstring baseUrl $ \(Right res) ->
      testRunSourceIO res `shouldReturn` Right [alice, bob, alice]
