{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.Cache.Internal.DynMap
-- Copyright   :  (C) 2015 Futurice Oy
-- License     :  BSD3
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Concurrent dictionary with  dynamic keys.
module Servant.Cache.Internal.DynMap (
   DynMap,
   newIO,
   insert,
   lookup,
   size,
   ) where

import Prelude        ()
import Prelude.Compat hiding (lookup)

import Control.Concurrent.STM (STM)
import Data.Hashable          (Hashable)
import Data.Proxy             (Proxy (..))
import Data.Typeable          (TypeRep, Typeable, typeRep)
import Unsafe.Coerce          (unsafeCoerce)

import qualified ListT
import qualified STMContainers.Map as SMap

-- | Dynamic map: @fk k -> fv v@
--
-- With @fk = fv =@ 'Identity', 'DynMap' is truly dynamic.
-- With @fk = Const k@, @fv = Const v@, 'DynMap' is static.
--
-- /Invariant:/ outer map keys typereps are of the @k@ and @v@ of the inner
-- map.
newtype DynMap fk fv = DynMap (SMap.Map (TypeRep, TypeRep) (SubDynMap fk fv))
data SubDynMap fk fv =
    forall k v. (Hashable (fk k), Eq (fk k)) => SubDynMap !(SMap.Map (fk k) (fv v))

newIO :: IO (DynMap fk fv)
newIO = DynMap <$> SMap.newIO

lookup :: forall k v fk fv. (Typeable k, Eq (fk k), Hashable (fk k), Typeable v)
       => fk k -> Proxy v -> DynMap fk fv -> STM (Maybe (fv v))
lookup key _ (DynMap dmap) = do
    l <- SMap.lookup tr dmap
    case l of
        Nothing -> return Nothing
        Just (SubDynMap sdm) ->
            unsafeCoerce $ SMap.lookup (unsafeCoerce key) sdm
  where tr = (typeRep (Proxy :: Proxy k), typeRep (Proxy :: Proxy v))

insert :: forall k v fk fv. (Typeable k, Eq (fk k), Hashable (fk k), Typeable v)
       => fk k -> fv v -> DynMap fk fv -> STM ()
insert key value (DynMap dmap) = do
    l <- SMap.lookup tr dmap
    case l of
        Nothing -> do
            sdm <- SMap.new
            SMap.insert value key sdm
            SMap.insert (SubDynMap sdm) tr dmap
        Just (SubDynMap sdm) ->
            SMap.insert (unsafeCoerce value) (unsafeCoerce key) sdm
  where tr = (typeRep (Proxy :: Proxy k), typeRep (Proxy :: Proxy v))

size :: forall fk fv. DynMap fk fv -> STM Int
size (DynMap dmap) = ListT.fold outerSize 0 (SMap.stream dmap)
  where
    outerSize :: Int -> (k, SubDynMap fk fv) -> STM Int
    outerSize acc (_, SubDynMap sdm) = ListT.fold innerSize acc (SMap.stream sdm)

    innerSize :: Int -> (k, v) -> STM Int
    innerSize acc _ = pure (acc + 1)
