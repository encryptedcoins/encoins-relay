{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}

module Encoins.Relay.Common.Error.ToEnvelope where

import           Control.Exception                  (SomeException)
import           Control.Monad.Catch                (MonadCatch, handle)
import           Control.Monad.Except               (MonadError)
import           Data.Kind                          (Type)
import           Encoins.Common.Log                 (logInfoS)
import           Encoins.Relay.Common.Error.Class   (IsCardanoServerError, toServantError)
import           Encoins.Relay.Common.Error.Servant (Envelope (..))
import           Encoins.Relay.Common.Error.Utils   (All)
import           Katip                              (KatipContext)
import           Servant                            (ServerError, throwError)

toEnvelope :: forall es m a.
    (MonadCatch m,  KatipContext m, MonadError ServerError m, WithErrorHandlers es) => m a -> m (Envelope es a)
toEnvelope = withErrorHandlers @es . fmap SuccEnvelope

class All IsCardanoServerError es => WithErrorHandlers (es :: [Type]) where
    withErrorHandlers :: (MonadCatch m,  KatipContext m, MonadError ServerError m) => m (Envelope es' a) -> m (Envelope es' a)

instance WithErrorHandlers '[] where
    withErrorHandlers = handle $ \(e :: SomeException) -> logInfoS True e >> throwError (toServantError e)

instance (All IsCardanoServerError (e ': es), WithErrorHandlers es) => WithErrorHandlers (e ': es) where
    withErrorHandlers = handle reThrow . withErrorHandlers @es
        where
            reThrow e = logInfoS True e >> throwError (toServantError @e e)
