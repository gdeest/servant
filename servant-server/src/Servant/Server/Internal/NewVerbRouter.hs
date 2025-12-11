{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Router implementation for the unified 'NewVerb' type.
--
-- This module provides the routing infrastructure for 'NewVerb' endpoints,
-- implementing the M+N content negotiation model where success responses (2xx)
-- use strict negotiation and error responses (4xx/5xx) use lenient negotiation.
module Servant.Server.Internal.NewVerbRouter
  ( -- * Router Class
    VerbRouter (..)

    -- * Content Negotiation
  , NegotiationStrategy (..)
  , negotiationStrategy

    -- * Response Rendering
  , NewVerbResponseRender (..)
  , NewVerbResponseListRender (..)
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.SOP (I (..), NS (..))
import qualified Network.HTTP.Media as M
import Data.Maybe (fromMaybe)
import Network.HTTP.Types
  ( HeaderName
  , Method
  , Status
  , hAccept
  , hContentType
  , methodGet
  , methodHead
  , statusIsSuccessful
  )
import qualified Network.Wai as Wai

import Servant.API.ContentTypes
  ( AcceptHeader (..)
  , AllMime (..)
  , AllMimeRender (..)
  )
import Servant.API.Experimental.Verb
import Servant.API.MultiVerb (AsUnion (..))
import Servant.API.Status (KnownStatus, statusVal)
import Servant.API.UVerb.Union (Union)
import Servant.Server.Internal.Delayed (Delayed, addAcceptCheck, addMethodCheck, runAction)
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFail)
import Servant.Server.Internal.Handler (Handler)
import Servant.Server.Internal.Router (Router, leafRouter)
import Servant.Server.Internal.RouteResult (RouteResult (..))
import Servant.Server.Internal.ServerError (err405, err406)

--------------------------------------------------------------------------------
-- Content Negotiation Strategy
--------------------------------------------------------------------------------

-- | Content negotiation strategy.
--
-- The M+N model uses different strategies for different response types:
--
-- * 'Strict': Used for success responses (2xx). Returns 406 if no acceptable
--   content type is found.
--
-- * 'Lenient': Used for error responses (4xx/5xx). Falls back to the default
--   content type if no acceptable type is found. Never returns 406.
data NegotiationStrategy
  = -- | 406 if no match (for 2xx responses)
    Strict
  | -- | Best effort, use default if no match (for 4xx/5xx responses)
    Lenient
  deriving (Eq, Show)

-- | Determine negotiation strategy based on status code.
negotiationStrategy :: Status -> NegotiationStrategy
negotiationStrategy s
  | statusIsSuccessful s = Strict
  | otherwise = Lenient

--------------------------------------------------------------------------------
-- Router Class
--------------------------------------------------------------------------------

-- | Dispatch routing based on response classification.
--
-- This class enables a single 'HasServer' instance for 'NewVerb' that
-- dispatches to different implementations based on the structure of the
-- response type.
class VerbRouter (kind :: ResponseKind) (a :: Type) where
  routeVerb
    :: Proxy kind
    -> Proxy a
    -> Method
    -> Delayed env (Handler (HandlerReturn a))
    -> Router env

--------------------------------------------------------------------------------
-- Single Response Instance
--------------------------------------------------------------------------------

instance
  ( AllMime cts
  , AllMimeRender cts a
  , KnownStatus s
  )
  => VerbRouter 'SingleResponse (Responds s cts a)
  where
  routeVerb _ _ method action = leafRouter route'
    where
      status = statusVal (Proxy @s)

      route' env request respond = do
        let accH = getAcceptHeader request
        runAction
          ( action
              `addMethodCheck` methodCheck method request
              `addAcceptCheck` acceptCheck (Proxy @cts) accH
          )
          env
          request
          respond
          $ \output -> do
            case renderSingleResponse (Proxy @cts) accH output of
              Nothing -> FailFatal err406
              Just (contentT, body) ->
                let bdy = if allowedMethodHead method request then "" else body
                 in Route $ Wai.responseLBS status [(hContentType, contentT)] bdy

--------------------------------------------------------------------------------
-- Empty Response Instance
--------------------------------------------------------------------------------

instance
  KnownStatus s
  => VerbRouter 'EmptyResponse (RespondsEmpty s)
  where
  routeVerb _ _ method action = leafRouter route'
    where
      status = statusVal (Proxy @s)

      route' env request respond =
        runAction
          (action `addMethodCheck` methodCheck method request)
          env
          request
          respond
          $ \_output ->
            Route $ Wai.responseLBS status [] ""

--------------------------------------------------------------------------------
-- Multi-Response Instance
--------------------------------------------------------------------------------

instance
  ( AsUnion rs result
  , AllMime (SuccessContentTypes rs)
  , NewVerbResponseListRender rs
  )
  => VerbRouter 'MultiResponse (OneOf rs result)
  where
  routeVerb _ _ method action = leafRouter route'
    where
      route' env request respond = do
        let accH = getAcceptHeader request
        -- Accept check is against SUCCESS content types only (M+N model)
        runAction
          ( action
              `addMethodCheck` methodCheck method request
              `addAcceptCheck` acceptCheck (Proxy @(SuccessContentTypes rs)) accH
          )
          env
          request
          respond
          $ \result -> do
            let union = toUnion @rs result
            case renderUnionResponse @rs accH union of
              Nothing -> FailFatal err406
              Just (respStatus, headers, body) ->
                let bdy = if allowedMethodHead method request then "" else body
                 in Route $ Wai.responseLBS respStatus headers bdy

--------------------------------------------------------------------------------
-- Response Rendering
--------------------------------------------------------------------------------

-- | Render a single response with content negotiation.
renderSingleResponse
  :: forall cts a
   . AllMimeRender cts a
  => Proxy cts
  -> AcceptHeader
  -> a
  -> Maybe (B.ByteString, BSL.ByteString)
renderSingleResponse _ (AcceptHeader acc) x =
  M.mapAcceptMedia (map mkOutput (allMimeRender (Proxy @cts) x)) acc
  where
    mkOutput (mediaType, rendered) = (mediaType, (M.renderHeader mediaType, rendered))

-- | Render a response from the union type.
class NewVerbResponseRender (a :: Type) where
  -- | Render this response type.
  --
  -- Returns (status, headers, body)
  renderResponse
    :: AcceptHeader
    -> ResponseType a
    -> Maybe (Status, [(HeaderName, B.ByteString)], BSL.ByteString)

instance
  (AllMimeRender cts a, KnownStatus s)
  => NewVerbResponseRender (Responds s cts a)
  where
  renderResponse (AcceptHeader acc) x =
    let status = statusVal (Proxy @s)
        strategy = negotiationStrategy status
        renders = allMimeRender (Proxy @cts) x
     in case strategy of
          Strict ->
            -- Strict: must match Accept header
            M.mapAcceptMedia (map (mkOutput status) renders) acc
          Lenient ->
            -- Lenient: try to match, fall back to first content type
            case M.mapAcceptMedia (map (mkOutput status) renders) acc of
              Just r -> Just r
              Nothing -> case renders of
                [] -> Nothing
                ((mediaType, rendered) : _) ->
                  Just (status, [(hContentType, M.renderHeader mediaType)], rendered)
    where
      mkOutput s (mediaType, rendered) =
        (mediaType, (s, [(hContentType, M.renderHeader mediaType)], rendered))

instance
  KnownStatus s
  => NewVerbResponseRender (RespondsEmpty s)
  where
  renderResponse _ () =
    Just (statusVal (Proxy @s), [], "")

-- | Render a response from a union of response types.
class NewVerbResponseListRender (rs :: [Type]) where
  renderUnionResponse
    :: AcceptHeader
    -> Union (ResponseTypes rs)
    -> Maybe (Status, [(HeaderName, B.ByteString)], BSL.ByteString)

instance NewVerbResponseListRender '[] where
  renderUnionResponse _ x = case x of {}

instance
  ( NewVerbResponseRender r
  , NewVerbResponseListRender rs
  )
  => NewVerbResponseListRender (r ': rs)
  where
  renderUnionResponse acc (Z (I x)) = renderResponse @r acc x
  renderUnionResponse acc (S rest) = renderUnionResponse @rs acc rest

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

getAcceptHeader :: Wai.Request -> AcceptHeader
getAcceptHeader = AcceptHeader . fromMaybe "*/*" . lookup hAccept . Wai.requestHeaders

methodCheck :: Method -> Wai.Request -> DelayedIO ()
methodCheck method request
  | allowedMethod method request = pure ()
  | otherwise = delayedFail err405

allowedMethod :: Method -> Wai.Request -> Bool
allowedMethod method request =
  allowedMethodHead method request || Wai.requestMethod request == method

allowedMethodHead :: Method -> Wai.Request -> Bool
allowedMethodHead method request =
  method == methodGet && Wai.requestMethod request == methodHead

acceptCheck
  :: AllMime list
  => Proxy list
  -> AcceptHeader
  -> DelayedIO ()
acceptCheck proxy accH
  | canHandleAcceptH proxy accH = pure ()
  | otherwise = delayedFail err406

canHandleAcceptH :: AllMime list => Proxy list -> AcceptHeader -> Bool
canHandleAcceptH p (AcceptHeader h) = isJust $ M.matchAccept (allMime p) h
  where
    isJust Nothing = False
    isJust (Just _) = True
