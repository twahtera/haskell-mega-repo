{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module Control.Monad.CryptoRandom.Extra (
    CRandT,
    evalCRandT,
    runCRandT,
    GenError,
    HashDRBG,
    MonadInitHashDRBG(..),
    evalCRandTThrow,
    runCRandTThrow,
    ) where

-- Base compat
import Prelude        ()
import Prelude.Compat

-- Almost standard imports
import Control.Monad.Catch        (MonadThrow, throwM)
import Control.Monad.CryptoRandom (CRandT, evalCRandT, runCRandT)
import Control.Monad.IO.Class     (MonadIO (..))
import Crypto.Random.DRBG         (GenError, HashDRBG)
import Crypto.Random.DRBG.Hash    (instantiate)
import System.Entropy             (getEntropy)

-- | Monads which can initialise 'HashDRBG'
class Monad m => MonadInitHashDRBG m where
    -- | Make 'HashDRBG' seeded with 128 bytes of 'System.Entropy'.
    mkHashDRBG :: m HashDRBG

-- | We might want have more precise instances later,
-- but this is good for now.
instance (Monad m, MonadIO m) => MonadInitHashDRBG m where
    mkHashDRBG = liftIO (instantiate <$> getE <*> getE <*> pure "HashDRBG")
      where getE = getEntropy 64

-- | Helper around 'evalCRandT'.
--
-- > evalCRandTThrow m g = evalCRandT m g >>= either pure throwM
evalCRandTThrow
    :: MonadThrow m
     => CRandT g GenError m a -> g -> m a
evalCRandTThrow m g = evalCRandT m g >>= either throwM pure

-- | Helper around 'runCRandT'.
runCRandTThrow
    :: MonadThrow m
    => CRandT g GenError m a -> g -> m (a, g)
runCRandTThrow m g = runCRandT m g >>= either throwM pure
