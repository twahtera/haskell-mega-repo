{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module Control.Monad.PlanMill (
    MonadPlanMill(..),
    ForallFSymbol(..),
    ) where

import PlanMill.Internal.Prelude
import Prelude                   ()

import Control.Monad.Http               (MonadHttp)
import Control.Monad.Operational        (singleton)
import Control.Monad.Reader             (ReaderT (..))
import Data.Constraint                  (Constraint)
import Futurice.Constraint.ForallSymbol (ForallFSymbol (..))

import PlanMill.Classes
import PlanMill.Operational   (GenPlanMillT, PlanMillAction (..), runPlanMillT)
import PlanMill.Types.Request

-- | Class of monads capable to do planmill operations.
class (Applicative m, Monad m) => MonadPlanMill m where
    -- | Different planmill monads have different constraints
    type MonadPlanMillC m :: * -> Constraint

    -- | "Lift" planmill actions to monad action
    planmillAction :: MonadPlanMillC m a => PlanMill a -> m a

instance Monad m => MonadPlanMill (GenPlanMillT k m) where
    type MonadPlanMillC (GenPlanMillT k m) = k
    planmillAction = singleton . PlanMillAction

instance ( MonadIO m, MonadHttp m, MonadThrow m, MonadTime m, MonadLogger m
         , Applicative m
         , HasPlanMillBaseUrl env, HasCredentials env
         )
    => MonadPlanMill (ReaderT env m) where
    type MonadPlanMillC (ReaderT env m) = FromJSON
    planmillAction planmill =
        runPlanMillT $ planmillAction planmill
