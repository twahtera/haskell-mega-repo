{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | More strictly typed Haxl
module Haxl.Typed (
    -- * The monad and operations
    GenTyHaxl(..), runTyHaxl,
    -- ** TyEnv
    TyEnv,
    -- ** Building the TyEnv
    initTyEnv, emptyTyEnv,
    -- *** Building the StateStore
    TyStateStore, tyStateSet, tyStateEmpty,
    -- ** Exceptions
    MonadThrow(..), MonadCatch(..),
    -- ** Re-exports
    IsElem', IsSubset', Index, Image,
    ) where

import Prelude          ()
import Futurice.Prelude 

import Futurice.Has   (IsSubset', IsElem')
import Futurice.Peano (Image, Index)

import qualified Haxl.Core as Haxl

newtype GenTyHaxl (r :: [* -> *]) u a =
    GenTyHaxl { unTyHaxl :: Haxl.GenHaxl u a }

instance Functor (GenTyHaxl r u) where
    fmap f (GenTyHaxl x) = GenTyHaxl (fmap f x)

instance Applicative (GenTyHaxl r u) where
    pure = GenTyHaxl . pure
    GenTyHaxl f <*> GenTyHaxl x = GenTyHaxl (f <*> x)

instance Monad (GenTyHaxl r u) where
    return = pure
    GenTyHaxl x >>= f = GenTyHaxl (x >>= unTyHaxl . f)

runTyHaxl :: IsSubset' r' r (Image r' r) => TyEnv r u -> GenTyHaxl r' u a -> IO a
runTyHaxl (Tagged env) (GenTyHaxl haxl) = Haxl.runHaxl env haxl

type TyEnv (r :: [* -> *]) u = Tagged r (Haxl.Env u)

emptyTyEnv :: u -> IO (TyEnv '[] u)
emptyTyEnv = fmap Tagged . Haxl.emptyEnv

initTyEnv :: TyStateStore r -> u -> IO (TyEnv r u)
initTyEnv (Tagged ss) = fmap Tagged . Haxl.initEnv ss

type TyStateStore (r :: [* -> *]) = Tagged r Haxl.StateStore

tyStateEmpty :: TyStateStore '[]
tyStateEmpty = Tagged Haxl.stateEmpty

infixr 9 `tyStateSet`
tyStateSet :: Haxl.StateKey f
           => Haxl.State f
           -> TyStateStore fs
           -> TyStateStore (f ': fs)
tyStateSet s (Tagged ss) = Tagged (Haxl.stateSet s ss)

instance MonadThrow (GenTyHaxl r u) where
    throwM = GenTyHaxl . Haxl.throw

instance MonadCatch (GenTyHaxl r u) where
    catch (GenTyHaxl haxl) f = GenTyHaxl (Haxl.catch haxl (unTyHaxl . f))
