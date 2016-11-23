{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- This module is intended for interactive testing
module PlanMill.Test (
    evalPlanMillIO,
    ) where

import PlanMill.Internal.Prelude

import Control.Monad.Http   (evalHttpT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.CryptoRandom.Extra (MonadInitHashDRBG (..),
                                         evalCRandTThrow)

import PlanMill.Types.Cfg     (Cfg)
import PlanMill.Types.Request (PlanMill)
import PlanMill.Eval (evalPlanMill)

-- | Evaluate single PlanMill request
--
-- @
-- λ > :set -XOverloadedStrings
-- λ > :m +PlanMill.Test PlanMill.EndPoints.Timereports
-- λ > let cfg = Cfg (Ident 42) "secret" mockEndpoint)
-- λ > evalPlanMillIO cfg timereports
-- @
evalPlanMillIO
    :: FromJSON a
    => Cfg         -- ^ Configuration
    -> PlanMill a  -- ^ PlanMill request
    -> IO a
evalPlanMillIO cfg planmill = do
    g <- mkHashDRBG
    withStderrLogger $ \logger ->
        evalHttpT $ runLogT "evalPlanMillIO" logger $ flip runReaderT cfg $ flip evalCRandTThrow g $
            evalPlanMill planmill
