{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module  Servant.Client.Generic (
    AsClientT,
    GClient,
    genericClient,
    genericClientHoist,
    ) where

import           Data.Constraint
                 (Dict(..))
import           Data.Proxy
                 (Proxy (..))

import           Servant.API.Generic
import           Servant.Client.Core

-- | A type that specifies that an API record contains a client implementation.
data AsClientT (m :: * -> *)
instance GenericMode (AsClientT m) where
    type AsClientT m :- api = Client m api

-- | Generate a record of client functions.
genericClient
    :: forall routes m.
       ( HasClient (ToServantApi routes)
       , ClientConstraints (ToServantApi routes) m
       , GenericServant routes (AsClientT m)
       , Client m (ToServantApi routes) ~ ToServant routes (AsClientT m)
       )
    => routes (AsClientT m)
genericClient
    = fromServant
    $ clientIn (Proxy :: Proxy (ToServantApi routes)) (Proxy :: Proxy m)

-- | 'genericClient' but with 'hoistClientMonad' in between.
genericClientHoist
    :: forall routes m n.
       ( HasClient (ToServantApi routes)
       , ClientConstraints (ToServantApi routes) m
       , GenericServant routes (AsClientT n)
       , Client n (ToServantApi routes) ~ ToServant routes (AsClientT n)
       )
    => (forall x. m x -> n x)  -- ^ natural transformation
    -> routes (AsClientT n)
genericClientHoist nt
    = fromServant
    $ hoistClientMonad api nt
    $ clientIn api m
  where
    m = Proxy :: Proxy m
    api = Proxy :: Proxy (ToServantApi routes)

type GClientConstraints api m =
  ( GenericServant api (AsClientT m)
  , HasClient (ToServantApi api)
  -- , forall n. Client n (ToServantApi api) ~ ToServant api (AsClientT n)
  , Client m (ToServantApi api) ~ ToServant api (AsClientT m)
  )

class GClient (routes :: * -> *) where
  gClientDict :: Dict (GClientConstraints routes m)

  default gClientDict :: GClientConstraints routes m => Dict (GClientConstraints routes m)
  gClientDict = Dict

instance
  ( GClient api
  -- , forall m. Hoistable api m
  )
  => HasClient (GApi api) where
  type Client m (GApi api) = api (AsClientT m)
  type ClientConstraints (GApi api) m = ClientConstraints (ToServantApi api) m

  clientWithRoute :: forall m. ClientConstraints (GApi api) m => Proxy m -> Proxy (GApi api) -> Request -> Client m (GApi api)
  clientWithRoute pm _ request =
    case gClientDict @api @m of
      Dict -> fromServant $ clientWithRoute  pm (Proxy @(ToServantApi api)) request

  hoistClientMonad
    :: forall ma mb.
       Proxy (GApi api)
    -> (forall x. ma x -> mb x)
    -> Client ma (GApi api)
    -> Client mb (GApi api)
  hoistClientMonad _ nat clientA =
    case (gClientDict @api @ma, gClientDict @api @mb) of
      (Dict, Dict) ->
        fromServant $ hoistClientMonad @(ToServantApi api) @ma @mb Proxy nat $ toServant clientA
