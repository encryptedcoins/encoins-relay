{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}

module Encoins.Relay.Common.Error.Servant where

import           Data.Aeson.Types                 (ToJSON (..))
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Lazy.Char8       as LBS
import           Data.Kind                        (Type)
import qualified Data.List.NonEmpty               as NE
import           Data.Maybe                       (fromMaybe)
import           Encoins.Relay.Common.Error.Class (IsCardanoServerError (errStatus), cardanoServerErrorToJSON)
import           Encoins.Relay.Common.Error.Utils (Snoc)
import           GHC.TypeLits                     (KnownNat, Nat, natVal)
import           Network.HTTP.Types               (HeaderName, Method, Status, hAccept, hContentType, methodGet, methodHead)
import           Network.Wai                      (Request (requestHeaders, requestMethod), Response, ResponseReceived, responseLBS)
import           Servant                          (Handler, HasServer (..), IsMember, Proxy (..), ReflectMethod (..), Verb, err405, err406,
                                                   type (:>))
import           Servant.API                      (Accept (..), NoContent (..))
import           Servant.API.ContentTypes         (AcceptHeader (AcceptHeader), AllCTRender (handleAcceptH), AllMime (..),
                                                   AllMimeRender (..), canHandleAcceptH)
import           Servant.Client                   (HasClient (..))
import           Servant.Client.Core.RunClient    (RunClient)
import           Servant.Server.Internal          (RouteResult (FailFatal, Route), allowedMethod, ct_wildcard, delayedFail, leafRouter)
import           Servant.Server.Internal.Delayed  (Delayed, addAcceptCheck, addMethodCheck, runAction)
import           Servant.Server.Internal.Router   (Router')

------------------------------------------------ Servant combinators ------------------------------------------------

data Throws (e :: Type)

-- This is used internally and should not be used by end-users.
data Throwing (e :: [Type])

data VerbWithErrors
    (es :: [Type])
    (method :: k)
    (successStatusCode :: Nat)
    (contentTypes :: [Type])
    (a :: Type)

data Envelope es a
    = forall e. (IsMember e es, IsCardanoServerError e) => ErrEnvelope e | SuccEnvelope a

instance ToJSON a => ToJSON (Envelope es a) where
    toJSON = \case
        SuccEnvelope a -> toJSON a
        ErrEnvelope e  -> cardanoServerErrorToJSON e

type family ThrowingNonterminal api where
    ThrowingNonterminal (Throwing es :> Throws e :> api) = Throwing (Snoc es e) :> api
    ThrowingNonterminal (Throwing es :> c :> api) = c :> Throwing es :> api

------------------------------------------------ Servant boilerplate ------------------------------------------------

-- Throws:
-- Change Throws to Throwing
instance HasServer (Throwing '[e] :> api) ctx => HasServer (Throws e :> api) ctx where

    type ServerT (Throws e :> api) m = ServerT (Throwing '[e] :> api) m

    hoistServerWithContext _ =
        hoistServerWithContext (Proxy :: Proxy (Throwing '[e] :> api))

    route _ = route (Proxy :: Proxy (Throwing '[e] :> api))

instance (RunClient m, HasClient m (Throwing '[e] :> api)) => HasClient m (Throws e :> api) where

    type Client m (Throws e :> api) = Client m (Throwing '[e] :> api)

    clientWithRoute p Proxy = clientWithRoute p (Proxy @(Throwing '[e] :> api))

    hoistClientMonad pm _ = hoistClientMonad pm (Proxy @(Throwing '[e] :> api))

-- Throwing:
-- Change Throwing to VerbWithErrors
instance HasServer (VerbWithErrors es method status ctypes a) ctx
    => HasServer (Throwing es :> Verb method status ctypes a) ctx where

    type ServerT (Throwing es :> Verb method status ctypes a) m =
        ServerT (VerbWithErrors es method status ctypes a) m

    hoistServerWithContext _ =
        hoistServerWithContext (Proxy :: Proxy (VerbWithErrors es method status ctypes a))

    route _ = route
        (Proxy :: Proxy (VerbWithErrors es method status ctypes a))

instance (RunClient m, HasClient m (VerbWithErrors es method status ctypes a))
    => HasClient m (Throwing es :> Verb method status ctypes a) where

    type Client m (Throwing es :> Verb method status ctypes a) =
        Client m (VerbWithErrors es method status ctypes a)

    clientWithRoute p Proxy =
        clientWithRoute p (Proxy :: Proxy (VerbWithErrors es method status ctypes a))

    hoistClientMonad pm _ =
        hoistClientMonad pm (Proxy :: Proxy (VerbWithErrors es method status ctypes a))

-- When a Throws e comes immediately after a Throwing' es 'Snoc' the
-- e onto the es. Otherwise, if Throws e comes before any other
-- combinator, push it down so it is closer to the 'Verb'.
instance HasServer (ThrowingNonterminal (Throwing es :> api :> apis)) ctx
    => HasServer (Throwing es :> api :> apis) ctx where

    type ServerT (Throwing es :> api :> apis) m =
        ServerT (ThrowingNonterminal (Throwing es :> api :> apis)) m

    hoistServerWithContext _ =
        hoistServerWithContext (Proxy :: Proxy (ThrowingNonterminal (Throwing es :> api :> apis)))

    route _ = route (Proxy :: Proxy (ThrowingNonterminal (Throwing es :> api :> apis)))

instance (RunClient m, HasClient m (ThrowingNonterminal (Throwing es :> api :> apis)))
    => HasClient m (Throwing es :> api :> apis) where

    type Client m (Throwing es :> api :> apis) =
        Client m (ThrowingNonterminal (Throwing es :> api :> apis))

    clientWithRoute p _ =
        clientWithRoute p (Proxy :: Proxy (ThrowingNonterminal (Throwing es :> api :> apis)))

    hoistClientMonad pm _ =
        hoistClientMonad pm (Proxy @(ThrowingNonterminal (Throwing es :> api :> apis)))

-- VerbWithErrors:
instance
    ( KnownNat status
    , ReflectMethod method
    , AllCTRender ctypes (Envelope es a)
    ) => HasServer (VerbWithErrors es method status ctypes a) ctx where

    type ServerT (VerbWithErrors es method status ctypes a) m
        = m (Envelope es a)

    hoistServerWithContext _ _ nt = nt

    route Proxy _ = methodRouter method successStatus (Proxy :: Proxy ctypes)
        where
            method        = reflectMethod (Proxy :: Proxy method)
            successStatus = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

instance (RunClient m, HasClient m (Verb method status ctypes a))
    => HasClient m (VerbWithErrors es method status ctypes a) where

    type Client m (VerbWithErrors es method status ctypes a) =
        Client m (Verb method status ctypes a)

    clientWithRoute p _ = clientWithRoute p
        (Proxy :: Proxy (Verb method status ctypes a))

    hoistClientMonad pm _ =
        hoistClientMonad pm (Proxy @(Verb method status ctypes a))

methodRouter :: forall ctypes a es env.
    AllCTRender ctypes (Envelope es a)
    => Method
    -> Status
    -> Proxy ctypes
    -> Delayed env (Handler (Envelope es a))
    -> Router' env
        ( Request ->
          (RouteResult Response -> IO ResponseReceived)->
          IO ResponseReceived
        )
methodRouter method successStatus proxy action = leafRouter route'
    where
        route' env request respond = do
            let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
            let theAction = action
                  `addMethodCheck` methodCheck request
                  `addAcceptCheck` acceptCheck accH
            runAction theAction env request respond $ go request accH
        go request accH envel = do
            let status = case envel of
                    ErrEnvelope e  -> errStatus e
                    SuccEnvelope _ -> successStatus
            let handleA = handleAcceptH proxy (AcceptHeader accH) envel
            processMethodRouter handleA status method Nothing request
        methodCheck request
            | allowedMethod method request = return ()
            | otherwise                    = delayedFail err405
        acceptCheck accH
            | canHandleAcceptH proxy (AcceptHeader accH) = return ()
            | otherwise                                  = delayedFail err406

processMethodRouter
    :: Maybe (LBS.ByteString, LBS.ByteString)
    -> Status
    -> Method
    -> Maybe [(HeaderName, ByteString)]
    -> Request -> RouteResult Response
processMethodRouter handleA status method headers request = case handleA of
    Nothing -> FailFatal err406 -- this should not happen (checked before),
                                -- so we make it fatal if it does
    Just (contentT, body) -> 
        let bdy = if allowedMethodHead then "" else body
            hdrs = (hContentType, LBS.toStrict contentT) : fromMaybe [] headers
            allowedMethodHead = method == methodGet && requestMethod request == methodHead
        in  Route $ responseLBS status hdrs bdy

instance Accept ctyp => AllMimeRender '[ctyp] (Envelope es NoContent) where
    allMimeRender _ _ = map (, "") $ NE.toList $ contentTypes (Proxy :: Proxy ctyp)

instance AllMime (ctyp ': ctyp' ': ctyps) => AllMimeRender (ctyp ': ctyp' ': ctyps) (Envelope es NoContent) where
    allMimeRender p _ = zip (allMime p) (repeat "")
