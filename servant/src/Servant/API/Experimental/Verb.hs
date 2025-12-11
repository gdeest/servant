{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Experimental unified verb API for Servant 2.0.
--
-- This module provides a unified endpoint type that combines the functionality
-- of 'Verb', 'UVerb', and 'MultiVerb' into a single 2-parameter type where
-- content types and status codes are declared at the response level rather
-- than the endpoint level.
--
-- __This is an experimental API and may change in future versions.__
--
-- = Design Principles
--
-- 1. __Self-describing responses__: Each response carries its own status code
--    and content types.
--
-- 2. __M+N content negotiation__: Success responses (2xx) use strict negotiation
--    (406 on mismatch), while error responses (4xx/5xx) use lenient negotiation
--    (best effort, never 406).
--
-- 3. __Response classification__: The 'ClassifyResponse' type family enables
--    dispatch based on response structure.
--
-- = Example Usage
--
-- @
-- -- Simple endpoint returning JSON
-- type GetUser = NewVerb 'GET (Responds 200 '[JSON] User)
--
-- -- Multi-response endpoint
-- type GetUserOrError = NewVerb 'GET (OneOf
--     '[ Responds 200 '[JSON] User
--      , Responds 404 '[JSON] NotFoundError
--      ] (Either NotFoundError User))
-- @
module Servant.API.Experimental.Verb
  ( -- * Core Endpoint Type
    NewVerb

    -- * Response Descriptors
  , Responds
  , RespondsEmpty
  , Respond

    -- * Multi-Response
  , OneOf
  , OneOf'
  , DefaultResult

    -- * Type Families
  , StatusOf
  , ContentTypesOf
  , HandlerReturn
  , IsSuccess
  , SuccessContentTypes
  , AllContentTypes
  , ResponseTypes

    -- * Response Classification
  , ResponseKind (..)
  , ClassifyResponse

    -- * Convenience Type Synonyms
    -- ** 200 OK
  , Get'
  , Post'
  , Put'
  , Delete'
  , Patch'
    -- ** Multiple Content Types
  , GetMulti
  , PostMulti
  , PutMulti
  , DeleteMulti
  , PatchMulti
    -- ** 201 Created
  , PostCreated'
  , PutCreated'
    -- ** 202 Accepted
  , GetAccepted'
  , PostAccepted'
  , PutAccepted'
  , DeleteAccepted'
  , PatchAccepted'
    -- ** 204 No Content
  , GetNoContent'
  , PostNoContent'
  , PutNoContent'
  , DeleteNoContent'
  , PatchNoContent'
  ) where

import Data.Kind (Type)
import Data.Type.Bool (If, type (&&))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.TypeLits (ErrorMessage (..), Nat, TypeError, type (<=?))
import Network.HTTP.Types.Method (StdMethod (..))

import Servant.API.UVerb.Union (Union)

--------------------------------------------------------------------------------
-- Core Endpoint Type
--------------------------------------------------------------------------------

-- | Unified endpoint type for all HTTP verbs.
--
-- The response type @a@ is self-describing: it contains its own
-- status code(s) and content type(s).
--
-- @
-- type GetUser = NewVerb 'GET (Responds 200 '[JSON] User)
-- @
data NewVerb (method :: StdMethod) (a :: Type)
  deriving (Generic, Typeable)

--------------------------------------------------------------------------------
-- Response Descriptors
--------------------------------------------------------------------------------

-- | A single response with status, content types, and body.
--
-- The first content type in the list is the default/preferred format
-- used when content negotiation fails for error responses.
--
-- @
-- Responds 200 '[JSON] User        -- 200 OK with JSON body
-- Responds 404 '[JSON] NotFound    -- 404 with JSON error
-- @
data Responds (status :: Nat) (cts :: [Type]) (a :: Type)

-- | Response with no body (status only).
--
-- Used for 204 No Content, redirects, etc.
--
-- @
-- RespondsEmpty 204   -- 204 No Content
-- RespondsEmpty 301   -- 301 Moved Permanently (with Location header)
-- @
data RespondsEmpty (status :: Nat)

-- | Convenience alias for 'Responds' with a single content type.
--
-- @
-- Respond 200 JSON User  ===  Responds 200 '[JSON] User
-- @
type Respond (status :: Nat) (ct :: Type) (a :: Type) = Responds status '[ct] a

--------------------------------------------------------------------------------
-- Multi-Response
--------------------------------------------------------------------------------

-- | Multiple possible responses.
--
-- Each response in the list is self-describing with its own
-- status code and content types.
--
-- The @result@ type is what the handler returns; it's converted
-- to/from the response union via the 'AsUnion' class (from MultiVerb).
--
-- @
-- type UserEndpoint = NewVerb 'GET (OneOf
--     '[ Responds 200 '[JSON] User
--      , Responds 404 '[JSON] NotFound
--      ] (Either NotFound User))
-- @
data OneOf (rs :: [Type]) (result :: Type)

-- | Type-level helper: just the responses, result type inferred.
--
-- For simple cases where result type can be automatically derived.
type OneOf' (rs :: [Type]) = OneOf rs (DefaultResult rs)

-- | Infer a default result type from response types.
--
-- - Two responses: @Either@
-- - Otherwise: @Union@ of response types
type family DefaultResult (rs :: [Type]) :: Type where
  DefaultResult '[r1, r2] = Either (ResponseType r1) (ResponseType r2)
  DefaultResult rs = Union (ResponseTypes rs)

--------------------------------------------------------------------------------
-- Type Families for Response Introspection
--------------------------------------------------------------------------------

-- | Extract the status code from a response type.
type family StatusOf (a :: Type) :: Nat where
  StatusOf (Responds s cts a) = s
  StatusOf (RespondsEmpty s) = s
  StatusOf a =
    TypeError
      ( 'Text "Cannot determine status of " ':<>: 'ShowType a ':$$:
        'Text "Use Responds or RespondsEmpty to specify status"
      )

-- | Extract content types from a response type.
type family ContentTypesOf (a :: Type) :: [Type] where
  ContentTypesOf (Responds s cts a) = cts
  ContentTypesOf (RespondsEmpty s) = '[]
  ContentTypesOf (OneOf rs r) = AllContentTypes rs
  ContentTypesOf a =
    TypeError
      ( 'Text "Cannot determine content types of " ':<>: 'ShowType a
      )

-- | Extract the body type from a response descriptor.
type family ResponseType (a :: Type) :: Type where
  ResponseType (Responds s cts a) = a
  ResponseType (RespondsEmpty s) = ()
  ResponseType a =
    TypeError
      ( 'Text "Cannot determine response type of " ':<>: 'ShowType a ':$$:
        'Text "Use Responds or RespondsEmpty"
      )

-- | Map 'ResponseType' over a list of response descriptors.
type family ResponseTypes (rs :: [Type]) :: [Type] where
  ResponseTypes '[] = '[]
  ResponseTypes (r ': rs) = ResponseType r ': ResponseTypes rs

-- | Compute handler return type from a response specification.
--
-- - 'Responds': Returns the body type
-- - 'RespondsEmpty': Returns ()
-- - 'OneOf': Returns the result type
type family HandlerReturn (a :: Type) :: Type where
  HandlerReturn (Responds s cts a) = a
  HandlerReturn (RespondsEmpty s) = ()
  HandlerReturn (OneOf rs result) = result
  HandlerReturn a =
    TypeError
      ( 'Text "Cannot determine handler return type of " ':<>: 'ShowType a
      )

-- | Is this a success status code (2xx)?
type family IsSuccess (s :: Nat) :: Bool where
  IsSuccess s = (200 <=? s) && (s <=? 299)

-- | Collect content types from success responses only (2xx).
--
-- This is used for Accept header checking. Only success responses
-- participate in strict content negotiation.
type family SuccessContentTypes (rs :: [Type]) :: [Type] where
  SuccessContentTypes '[] = '[]
  SuccessContentTypes (Responds s cts a ': rest) =
    If
      (IsSuccess s)
      (UnionTypes cts (SuccessContentTypes rest))
      (SuccessContentTypes rest)
  SuccessContentTypes (RespondsEmpty s ': rest) =
    SuccessContentTypes rest -- No body, no content type

-- | Collect all content types from all responses (for documentation).
type family AllContentTypes (rs :: [Type]) :: [Type] where
  AllContentTypes '[] = '[]
  AllContentTypes (Responds s cts a ': rest) =
    UnionTypes cts (AllContentTypes rest)
  AllContentTypes (RespondsEmpty s ': rest) =
    AllContentTypes rest

-- | Union of two type-level lists (removes duplicates).
type family UnionTypes (xs :: [Type]) (ys :: [Type]) :: [Type] where
  UnionTypes '[] ys = ys
  UnionTypes (x ': xs) ys =
    If (ElemType x ys) (UnionTypes xs ys) (x ': UnionTypes xs ys)

-- | Check if a type is an element of a type-level list.
type family ElemType (x :: Type) (xs :: [Type]) :: Bool where
  ElemType x '[] = 'False
  ElemType x (x ': xs) = 'True
  ElemType x (y ': xs) = ElemType x xs

--------------------------------------------------------------------------------
-- Response Classification
--------------------------------------------------------------------------------

-- | Classification of response types for instance dispatch.
--
-- This enables a single 'HasServer'/'HasClient' instance that
-- dispatches to different implementations based on response structure.
data ResponseKind
  = -- | Single response with body: @Responds s cts a@
    SingleResponse
  | -- | Empty response: @RespondsEmpty s@
    EmptyResponse
  | -- | Multiple possible responses: @OneOf rs result@
    MultiResponse

-- | Classify a response type for instance dispatch.
type family ClassifyResponse (a :: Type) :: ResponseKind where
  ClassifyResponse (OneOf rs r) = 'MultiResponse
  ClassifyResponse (RespondsEmpty s) = 'EmptyResponse
  ClassifyResponse (Responds s cts a) = 'SingleResponse
  ClassifyResponse a =
    TypeError
      ( 'Text "Unknown response type: " ':<>: 'ShowType a ':$$:
        'Text "Use Responds, RespondsEmpty, or OneOf"
      )

--------------------------------------------------------------------------------
-- Convenience Type Synonyms
--------------------------------------------------------------------------------

-- ** 200 OK (single content type)

-- | GET with 200 OK
type Get' ct a = NewVerb 'GET (Responds 200 '[ct] a)

-- | POST with 200 OK
type Post' ct a = NewVerb 'POST (Responds 200 '[ct] a)

-- | PUT with 200 OK
type Put' ct a = NewVerb 'PUT (Responds 200 '[ct] a)

-- | DELETE with 200 OK
type Delete' ct a = NewVerb 'DELETE (Responds 200 '[ct] a)

-- | PATCH with 200 OK
type Patch' ct a = NewVerb 'PATCH (Responds 200 '[ct] a)

-- ** 200 OK (multiple content types)

-- | GET with 200 OK and multiple content types
type GetMulti cts a = NewVerb 'GET (Responds 200 cts a)

-- | POST with 200 OK and multiple content types
type PostMulti cts a = NewVerb 'POST (Responds 200 cts a)

-- | PUT with 200 OK and multiple content types
type PutMulti cts a = NewVerb 'PUT (Responds 200 cts a)

-- | DELETE with 200 OK and multiple content types
type DeleteMulti cts a = NewVerb 'DELETE (Responds 200 cts a)

-- | PATCH with 200 OK and multiple content types
type PatchMulti cts a = NewVerb 'PATCH (Responds 200 cts a)

-- ** 201 Created

-- | POST with 201 Created
type PostCreated' ct a = NewVerb 'POST (Responds 201 '[ct] a)

-- | PUT with 201 Created
type PutCreated' ct a = NewVerb 'PUT (Responds 201 '[ct] a)

-- ** 202 Accepted

-- | GET with 202 Accepted
type GetAccepted' ct a = NewVerb 'GET (Responds 202 '[ct] a)

-- | POST with 202 Accepted
type PostAccepted' ct a = NewVerb 'POST (Responds 202 '[ct] a)

-- | PUT with 202 Accepted
type PutAccepted' ct a = NewVerb 'PUT (Responds 202 '[ct] a)

-- | DELETE with 202 Accepted
type DeleteAccepted' ct a = NewVerb 'DELETE (Responds 202 '[ct] a)

-- | PATCH with 202 Accepted
type PatchAccepted' ct a = NewVerb 'PATCH (Responds 202 '[ct] a)

-- ** 204 No Content

-- | GET with 204 No Content
type GetNoContent' = NewVerb 'GET (RespondsEmpty 204)

-- | POST with 204 No Content
type PostNoContent' = NewVerb 'POST (RespondsEmpty 204)

-- | PUT with 204 No Content
type PutNoContent' = NewVerb 'PUT (RespondsEmpty 204)

-- | DELETE with 204 No Content
type DeleteNoContent' = NewVerb 'DELETE (RespondsEmpty 204)

-- | PATCH with 204 No Content
type PatchNoContent' = NewVerb 'PATCH (RespondsEmpty 204)
