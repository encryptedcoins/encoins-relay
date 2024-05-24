{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module Encoins.Relay.Common.Error.Utils where

import           Data.Kind (Constraint, Type)

type family All (constr :: Type -> Constraint) (xs :: [Type]) :: Constraint where
    All _ '[]       = ()
    All c (x ': xs) = (c x, All c xs)

type family Snoc (as :: [k]) (b :: k) where
    Snoc '[] b = '[b]
    Snoc (a ': as) b = (a ': Snoc as b)
