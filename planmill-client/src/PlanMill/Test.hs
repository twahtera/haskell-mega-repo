{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- This module is intended for interactive testing
module PlanMill.Test (
    evalPlanMillIO,
    module PlanMill,
    ) where

import Prelude        ()
import Prelude.Compat

import Control.Monad.Http   (evalHttpT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.Aeson.Compat    (FromJSON)

import Control.Monad.PlanMill (planmillAction)
import PlanMill               (Cfg)
import PlanMill.Types.Request (PlanMill)

-- | Evaluate single PlanMill request
--
-- @
-- λ > :set -XOverloadedStrings
-- λ > :m +PlanMill.Test PlanMill.EndPoints.Timereports
-- λ > let cfg = Cfg (Ident 42) "secret" mockEndpoint)
-- λ > evalPlanMillIO cfg timereports
-- @
evalPlanMillIO :: forall a. FromJSON a
               => Cfg         -- ^ Configuration
               -> PlanMill a  -- ^ PlanMill request
               -> IO a
evalPlanMillIO cfg planmill =
    evalHttpT $ runStderrLoggingT $ runReaderT (planmillAction planmill) cfg
