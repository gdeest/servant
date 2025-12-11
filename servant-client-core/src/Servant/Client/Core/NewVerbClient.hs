{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Client implementation for the unified 'NewVerb' type.
--
-- This module provides the client-side infrastructure for 'NewVerb' endpoints,
-- implementing response parsing for single, empty, and multi-response endpoints.
module Servant.Client.Core.NewVerbClient
  ( -- * Client Class
    VerbClient (..)
  , VerbClientConstraint

    -- * Response Unrendering
  , NewVerbResponseUnrender (..)
  , NewVerbResponseListUnrender (..)
  ) where

import Control.Applicative (Alternative (..), (<|>))
import Control.Monad (guard, unless)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (toList)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.SOP (I (..), NS (..))
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import GHC.TypeLits (Nat)
import qualified Network.HTTP.Media as M
import Network.HTTP.Types (Method, Status)

import Data.Constraint (Constraint)
import Servant.API.ContentTypes
  ( AllMime (..)
  , AllMimeUnrender (..)
  , MimeUnrender (..)
  , contentType
  )
import Servant.API.Experimental.Verb
import Servant.API.Stream (FramingUnrender (..), FromSourceIO (..))
import Servant.API.MultiVerb
  ( AsUnion (..)
  , UnrenderResult (..)
  )
import Servant.API.Status (KnownStatus, statusVal)
import Servant.API.UVerb.Union (Union)
import Servant.Client.Core.ClientError (ClientError (..))
import Servant.Client.Core.MultiVerb.ResponseUnrender
  ( SomeClientResponse (..)
  , fromSomeClientResponse
  )
import Servant.Client.Core.Request (Request, requestAccept, requestMethod)
import Servant.Client.Core.Response (Response, ResponseF (..))
import qualified Servant.Client.Core.Response as Response
import Servant.Client.Core.RunClient (RunClient (..), RunStreamingClient (..))

--------------------------------------------------------------------------------
-- Client Class
--------------------------------------------------------------------------------

-- | Constraint required for a given response kind.
--
-- * Non-streaming responses require 'RunClient'
-- * Streaming responses require 'RunStreamingClient'
type family VerbClientConstraint (kind :: ResponseKind) (m :: Type -> Type) :: Constraint where
  VerbClientConstraint 'StreamingResponse m = RunStreamingClient m
  VerbClientConstraint _ m = RunClient m

-- | Dispatch client generation based on response classification.
--
-- This class enables a single 'HasClient' instance for 'NewVerb' that
-- dispatches to different implementations based on the structure of the
-- response type.
--
-- The constraint varies by response kind: streaming responses require
-- 'RunStreamingClient', while others require 'RunClient'.
class VerbClient (kind :: ResponseKind) (a :: Type) where
  clientVerb
    :: VerbClientConstraint kind m
    => Proxy kind
    -> Proxy a
    -> Method
    -> Request
    -> m (HandlerReturn a)

--------------------------------------------------------------------------------
-- Single Response Instance
--------------------------------------------------------------------------------

instance
  ( AllMime cts
  , AllMimeUnrender cts a
  , KnownStatus s
  )
  => VerbClient 'SingleResponse (Responds s cts a)
  where
  clientVerb _ _ method req = do
    response@Response{responseBody = body} <-
      runRequestAcceptStatus
        (Just [statusVal (Proxy @s)])
        req
          { requestMethod = method
          , requestAccept = Seq.fromList accept
          }

    contentType <- getResponseContentType response
    unless (any (M.matches contentType) accept) $
      throwClientError $ UnsupportedContentType contentType response

    case unrenderResponse @cts contentType body of
      Left err -> throwClientError $ DecodeFailure (Text.pack err) response
      Right val -> pure val
    where
      accept = allMime (Proxy @cts)

--------------------------------------------------------------------------------
-- Empty Response Instance
--------------------------------------------------------------------------------

instance
  KnownStatus s
  => VerbClient 'EmptyResponse (RespondsEmpty s)
  where
  clientVerb _ _ method req = do
    _ <-
      runRequestAcceptStatus
        (Just [statusVal (Proxy @s)])
        req{requestMethod = method}
    pure ()

--------------------------------------------------------------------------------
-- Multi-Response Instance
--------------------------------------------------------------------------------

instance
  ( AsUnion rs result
  , AllMime (SuccessContentTypes rs)
  , NewVerbResponseListUnrender rs
  )
  => VerbClient 'MultiResponse (OneOf rs result)
  where
  clientVerb _ _ method req = do
    response@Response{responseBody = body} <-
      runRequestAcceptStatus
        (Just (newVerbResponseStatuses @rs))
        req
          { requestMethod = method
          , requestAccept = Seq.fromList accept
          }

    contentType <- getResponseContentType response
    unless (any (M.matches contentType) accept || null accept) $
      throwClientError $ UnsupportedContentType contentType response

    let sresp =
          if BSL.null body
            then SomeClientResponse $ response{Response.responseBody = ()}
            else SomeClientResponse response
    case newVerbResponseListUnrender @rs contentType sresp of
      StatusMismatch -> throwClientError (DecodeFailure "Status mismatch" response)
      UnrenderError e -> throwClientError (DecodeFailure (Text.pack e) response)
      UnrenderSuccess x -> pure (fromUnion @rs x)
    where
      accept = allMime (Proxy @(SuccessContentTypes rs))

--------------------------------------------------------------------------------
-- Streaming Response Instance
--------------------------------------------------------------------------------

instance
  ( FramingUnrender framing
  , FromSourceIO chunk (SourceIO chunk)
  , MimeUnrender ct chunk
  )
  => VerbClient 'StreamingResponse (RespondsStream s framing ct chunk)
  where
  clientVerb _ _ method req =
    withStreamingRequest req' $ \Response{responseBody = body} -> do
      let mimeUnrender' = mimeUnrender (Proxy @ct)
          framingUnrender' = framingUnrender (Proxy @framing) mimeUnrender'
      fromSourceIO @chunk @(SourceIO chunk) $ framingUnrender' body
    where
      req' =
        req
          { requestAccept = Seq.fromList [contentType (Proxy @ct)]
          , requestMethod = method
          }

--------------------------------------------------------------------------------
-- Response Unrendering
--------------------------------------------------------------------------------

-- | Unrender a response body using content negotiation.
unrenderResponse
  :: forall cts a
   . AllMimeUnrender cts a
  => M.MediaType
  -> BSL.ByteString
  -> Either String a
unrenderResponse contentType body =
  let renders = allMimeUnrender (Proxy @cts)
   in case lookup contentType renders of
        Nothing ->
          -- Try matching instead of exact lookup
          case filter (\(mt, _) -> contentType `M.matches` mt) renders of
            [] -> Left $ "No decoder for content type: " ++ show contentType
            ((_, decoder) : _) -> decoder body
        Just decoder -> decoder body

-- | Unrender a single response type.
class NewVerbResponseUnrender (a :: Type) where
  type NewVerbResponseBody a :: Type
  type NewVerbResponseStatus a :: Nat
  newVerbResponseUnrender
    :: M.MediaType
    -> ResponseF (NewVerbResponseBody a)
    -> UnrenderResult (ResponseType a)

instance
  (AllMimeUnrender cts a, KnownStatus s)
  => NewVerbResponseUnrender (Responds s cts a)
  where
  type NewVerbResponseBody (Responds s cts a) = BSL.ByteString
  type NewVerbResponseStatus (Responds s cts a) = s

  newVerbResponseUnrender contentType response = do
    guard (Response.responseStatusCode response == statusVal (Proxy @s))
    let renders = allMimeUnrender (Proxy @cts)
    case lookup contentType renders of
      Nothing ->
        -- Try matching instead of exact lookup
        case filter (\(mt, _) -> contentType `M.matches` mt) renders of
          [] -> empty
          ((_, decoder) : _) ->
            either UnrenderError UnrenderSuccess (decoder (Response.responseBody response))
      Just decoder ->
        either UnrenderError UnrenderSuccess (decoder (Response.responseBody response))

instance
  KnownStatus s
  => NewVerbResponseUnrender (RespondsEmpty s)
  where
  type NewVerbResponseBody (RespondsEmpty s) = ()
  type NewVerbResponseStatus (RespondsEmpty s) = s

  newVerbResponseUnrender _ response =
    guard (Response.responseStatusCode response == statusVal (Proxy @s))

-- | Unrender from a union of response types.
class Typeable rs => NewVerbResponseListUnrender (rs :: [Type]) where
  newVerbResponseListUnrender
    :: M.MediaType
    -> SomeClientResponse
    -> UnrenderResult (Union (ResponseTypes rs))

  newVerbResponseStatuses :: [Status]

instance NewVerbResponseListUnrender '[] where
  newVerbResponseListUnrender _ _ = StatusMismatch
  newVerbResponseStatuses = []

instance
  ( KnownStatus (StatusOf r)
  , NewVerbResponseListUnrender rs
  , NewVerbResponseUnrender r
  , Typeable (NewVerbResponseBody r)
  , Typeable r
  )
  => NewVerbResponseListUnrender (r ': rs)
  where
  newVerbResponseListUnrender contentType output =
    Z . I <$> (newVerbResponseUnrender @r contentType =<< fromSomeClientResponse output)
      <|> S <$> newVerbResponseListUnrender @rs contentType output

  newVerbResponseStatuses =
    statusVal (Proxy @(StatusOf r)) : newVerbResponseStatuses @rs

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

getResponseContentType :: RunClient m => Response -> m M.MediaType
getResponseContentType response =
  case lookup "Content-Type" (toList (Response.responseHeaders response)) of
    Nothing -> pure $ "application" M.// "octet-stream"
    Just t -> case M.parseAccept t of
      Nothing -> throwClientError $ InvalidContentTypeHeader response
      Just t' -> pure t'
