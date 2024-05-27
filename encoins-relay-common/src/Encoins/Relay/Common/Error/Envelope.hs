{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveFunctor             #-}
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
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}

module Encoins.Relay.Common.Error.Envelope where

import           Data.Aeson                       (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson                       as J
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Lazy.Char8       as LBS
import           Data.Kind                        (Type)
import qualified Data.List.NonEmpty               as NE
import           Data.Maybe                       (fromMaybe)
import           Data.WorldPeace                  (Contains, OpenUnion, absurdUnion, openUnion, openUnionLift, relaxUnion)
import           Encoins.Relay.Common.Error.Class (IsCardanoServerError (errStatus), cardanoServerErrorFromJSON, cardanoServerErrorToJSON)
import           Encoins.Relay.Common.Error.Utils (All, Snoc)
import           GHC.TypeLits                     (KnownNat, Nat, natVal)
import           Network.HTTP.Types               (HeaderName, Method, Status, hAccept, hContentType, methodGet, methodHead)
import           Network.Wai                      (Request (requestHeaders, requestMethod), Response, ResponseReceived, responseLBS)
import           Servant                          (Handler, HasServer (..), MimeUnrender, Proxy (..), ReflectMethod (..), Verb, err405, err406, type (:>))
import           Servant.API                      (Accept (..), NoContent (..), JSON)
import           Servant.API.ContentTypes         (AcceptHeader (AcceptHeader), AllCTRender (handleAcceptH), AllMime (..),
                                                   AllMimeRender (..), MimeUnrender (..), canHandleAcceptH)
import           Servant.Client                   (HasClient (..))
import           Servant.Client.Core.RunClient    (RunClient)
import           Servant.Server.Internal          (RouteResult (FailFatal, Route), allowedMethod, ct_wildcard, delayedFail, leafRouter)
import           Servant.Server.Internal.Delayed  (Delayed, addAcceptCheck, addMethodCheck, runAction)
import           Servant.Server.Internal.Router   (Router')


data Envelope es a
    = (All IsCardanoServerError es) => ErrEnvelope (OpenUnion es) | SuccEnvelope a
deriving instance Functor (Envelope es)

envelopeToEither :: Envelope '[e] a -> Either e a
envelopeToEither = \case
    SuccEnvelope a -> Right a
    ErrEnvelope es -> Left $ openUnion absurdUnion id es

instance (Show (Envelope es a), Show a) => Show (Envelope (e : es) a) where
    show = \case
        SuccEnvelope a -> "SuccEnvelope " <> show a
        ErrEnvelope es -> openUnion (show . ErrEnvelope @_ @a) show es

instance Show a => Show (Envelope '[] a) where
    show = \case
        SuccEnvelope a -> "SuccEnvelope " <> show a
        ErrEnvelope es -> absurdUnion es

type EndpointEnvelope e = MkEndpointEnvelope (EndpointErrors e) (EndpointResult e)

type family EndpointResult e where
    EndpointResult (Verb _ _ _ a) = a
    EndpointResult (_ :> x) = EndpointResult x

type family EndpointErrors e where
    EndpointErrors (Throws err :> e) = err ': EndpointErrors e
    EndpointErrors (_          :> e) = EndpointErrors e
    EndpointErrors _                 = '[]

type family MkEndpointEnvelope es a where
    MkEndpointEnvelope '[]  a = a
    MkEndpointEnvelope  es  a = Envelope es a

class FromErrEnvelope es where
    errEnvelopeStatus :: OpenUnion es -> Status

instance FromErrEnvelope '[] where
    errEnvelopeStatus = absurdUnion

instance (All IsCardanoServerError (e ': es), FromErrEnvelope es) => FromErrEnvelope (e ': es) where
    errEnvelopeStatus = openUnion errEnvelopeStatus errStatus

mapUnion :: (All IsCardanoServerError es') => (OpenUnion es -> OpenUnion es') -> Envelope es a -> Envelope es' a
mapUnion f = \case
    SuccEnvelope s    -> SuccEnvelope s
    ErrEnvelope union -> ErrEnvelope $ f union

instance ToJSON a => ToJSON (Envelope '[] a) where
    toJSON = \case
        SuccEnvelope a    -> toJSON a
        ErrEnvelope union -> absurdUnion union

instance (ToJSON a, ToJSON (Envelope es a)) => ToJSON (Envelope (e : es) a) where
    toJSON = \case
        SuccEnvelope a    -> toJSON a
        ErrEnvelope union -> openUnion (toJSON . ErrEnvelope @es @a) (cardanoServerErrorToJSON @e) union

instance FromJSON a => FromJSON (Envelope '[] a) where
    parseJSON j = SuccEnvelope <$> parseJSON j

instance (All IsCardanoServerError (e : es), FromJSON (Envelope es a), Contains es (e : es)) => FromJSON (Envelope (e : es) a) where
    parseJSON j = maybe
        (mapUnion relaxUnion <$> parseJSON @(Envelope es a) j)
        (pure . ErrEnvelope . openUnionLift)
        (cardanoServerErrorFromJSON @e j)

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

type family ThrowingNonterminal api where
    ThrowingNonterminal (Throwing es :> Throws e :> api) = Throwing (Snoc es e) :> api
    ThrowingNonterminal (Throwing es :> c :> api) = c :> Throwing es :> api

------------------------------------------------ Servant boilerplate ------------------------------------------------

-- Throws:
-- Change Throws to Throwing
instance HasServer api ctx => HasServer (Throws e :> api) ctx where

    type ServerT (Throws e :> api) m = ServerT api m

    hoistServerWithContext _ =
        hoistServerWithContext (Proxy :: Proxy api)

    route _ = route (Proxy :: Proxy api)

instance (RunClient m, HasClient m (Throwing '[e] :> api)) => HasClient m (Throws e :> api) where

    type Client m (Throws e :> api) = Client m (Throwing '[e] :> api)

    clientWithRoute p Proxy = clientWithRoute p (Proxy @(Throwing '[e] :> api))

    hoistClientMonad pm _ = hoistClientMonad pm (Proxy @(Throwing '[e] :> api))

instance (RunClient m, HasClient m (VerbWithErrors es method status ctypes a))
    => HasClient m (Throwing es :> Verb method status ctypes a) where

    type Client m (Throwing es :> Verb method status ctypes a) =
        Client m (VerbWithErrors es method status ctypes a)

    clientWithRoute p Proxy =
        clientWithRoute p (Proxy :: Proxy (VerbWithErrors es method status ctypes a))

    hoistClientMonad pm _ =
        hoistClientMonad pm (Proxy :: Proxy (VerbWithErrors es method status ctypes a))
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
    , FromErrEnvelope es
    ) => HasServer (VerbWithErrors es method status ctypes a) ctx where

    type ServerT (VerbWithErrors es method status ctypes a) m
        = m (Envelope es a)

    hoistServerWithContext _ _ nt = nt

    route Proxy _ = methodRouter method successStatus (Proxy :: Proxy ctypes)
        where
            method        = reflectMethod (Proxy :: Proxy method)
            successStatus = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

-- ErrEnvelope '[] transforms to a
instance (RunClient m, HasClient m (Verb method status ctypes a))
    => HasClient m (VerbWithErrors '[] method status ctypes a) where

    type Client m (VerbWithErrors '[] method status ctypes a) =
        Client m (Verb method status ctypes a)

    clientWithRoute p _ = clientWithRoute p
        (Proxy :: Proxy (Verb method status ctypes a))

    hoistClientMonad pm _ =
        hoistClientMonad pm (Proxy @(Verb method status ctypes a))

-- ErrEnvelope (e1:e2:es) stays as ErrEnvelope
instance (RunClient m, HasClient m (Verb method status ctypes (Envelope (e1:es) a)))
    => HasClient m (VerbWithErrors (e1:es) method status ctypes a) where

    type Client m (VerbWithErrors (e1:es) method status ctypes a) =
        Client m (Verb method status ctypes (Envelope (e1:es) a))

    clientWithRoute p _ = clientWithRoute p
        (Proxy :: Proxy (Verb method status ctypes (Envelope (e1:es) a)))

    hoistClientMonad pm _ =
        hoistClientMonad pm (Proxy @(Verb method status ctypes (Envelope (e1:es) a)))

methodRouter :: forall ctypes a es env.
    (AllCTRender ctypes (Envelope es a), FromErrEnvelope es)
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
                    ErrEnvelope e  -> errEnvelopeStatus e
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
    Just (contentT, body) -> Route $ responseLBS status hdrs bdy
        where
            bdy = if allowedMethodHead then "" else body
            hdrs = (hContentType, LBS.toStrict contentT) : fromMaybe [] headers
            allowedMethodHead = method == methodGet && requestMethod request == methodHead

instance Accept ctyp => AllMimeRender '[ctyp] (Envelope es NoContent) where
    allMimeRender _ _ = map (, "") $ NE.toList $ contentTypes (Proxy :: Proxy ctyp)

instance AllMime (ctyp ': ctyp' ': ctyps) => AllMimeRender (ctyp ': ctyp' ': ctyps) (Envelope es NoContent) where
    allMimeRender p _ = zip (allMime p) (repeat "")

instance {-# OVERLAPPING #-} (All IsCardanoServerError (e : es), FromJSON (Envelope es ()), Contains es (e : es)) => MimeUnrender JSON (Envelope (e : es) NoContent) where
    mimeUnrender _ "" = Right $ SuccEnvelope NoContent
    mimeUnrender _ bs = (NoContent <$) <$> J.eitherDecode @(Envelope (e : es) ()) bs