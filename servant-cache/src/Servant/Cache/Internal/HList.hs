{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.Cache.Internal.HList
-- Copyright   :  (C) 2015 Futurice Oy
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Todo use @Generics.SOP.Curry@
module Servant.Cache.Internal.HList where

import Data.Hashable (Hashable (..))
import Data.Proxy    (Proxy (..))
import Data.Typeable (Typeable)
import GHC.Exts
--------------------------------------------------------------------------------
-- Type level trickery
------------------------------------------------------------------------------

-- | GADT Heterogenous list
data HList as where
   HNil   :: HList '[]
   HCons ::  a -> HList as -> HList (a ': as)
   deriving (Typeable)

deriving instance All Eq as => Eq (HList as)

instance All Hashable as => Hashable (HList as) where
    hashWithSalt salt HNil          = salt `hashWithSalt` (0::Int)
    hashWithSalt salt (HCons x xs)  = salt `hashWithSalt` (1::Int)
                                           `hashWithSalt` x
                                           `hashWithSalt` xs

-- | As in @HList@.
--
-- *Note* @f@ and @r@ do not determine @xs@, ie. there is no @f r -> xs@.
-- Or it's GHC which do not accept the dependency :(
--
-- @
-- (x -> b) r
-- c        c
-- -- Unify:
-- c ~ x -> b ~ r
-- @
class HCurry f xs r | f xs -> r , r xs -> f where
    hUncurry :: Proxy xs -> f -> HList xs -> r
    hCurry   :: Proxy xs -> (HList xs -> r) -> f

instance HCurry b '[] b where
    hUncurry _ b HNil = b
    hCurry _ f = f HNil

instance HCurry b xs r => HCurry (x -> b) (x ': xs) r where
    hUncurry _ f (HCons x xs) = hUncurry Proxy (f x) xs
    hCurry _ f x = hCurry Proxy (f . HCons x)

-- | As in @generics-sop-1.x@
type family All (c :: * -> Constraint) (xs :: [*]) :: Constraint where
    All c '[]       = ()
    All c (x ': xs) = (c x, All c xs)

-- | Pairs are trivially typeable
class PHList xs where
    type PList xs :: *
    toPList :: HList xs -> PList xs

instance PHList '[] where
    type PList '[] = ()
    toPList _ = ()

instance PHList xs => PHList (x ': xs) where
    type PList (x ': xs) = (x, PList xs)
    toPList (HCons x xs) = (x, toPList xs)
