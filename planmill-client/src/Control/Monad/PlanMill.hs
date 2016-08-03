{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE TypeSynonymInstances    #-}
{-# LANGUAGE UndecidableInstances    #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
#endif
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module Control.Monad.PlanMill (
    MonadPlanMill(..),
    planmillVectorAction,
    MonadPlanMillTypes,
    ForallFSymbol(..),
    ) where

import PlanMill.Internal.Prelude
import Prelude ()

import Control.Monad.Http               (MonadHttp)
import Control.Monad.Reader             (ReaderT (..))
import Data.Constraint                  (Constraint, Dict (..), type (:-)(..), (\\))
import Futurice.Constraint.ForallSymbol (ForallFSymbol (..))

import PlanMill.Classes
import PlanMill.Types
import PlanMill.Test (evalPlanMillIO)

-- | Types 'MonadPlanMillC' should be satisfied to define 'MonadPlanMill' instance.
--
-- Requiring these reduces boilerplate greatly!
type MonadPlanMillTypes =
    '[ Absence, Assignment, Me, Meta, Project, ReportableAssignment
     , Task, TimeBalance, Timereport, Team, User, UserCapacity
     ]
-- Note: to update do:
-- intercalate ", " $ sort $ splitOn ", " "User, Team"

-- | Class of monads capable to do planmill operations.
class
    ( Applicative m, Monad m
    -- Unfortunately we have to write all of those down
    , MonadPlanMillC m Absence
    , MonadPlanMillC m Assignment
    , MonadPlanMillC m Me
    , MonadPlanMillC m Meta
    , MonadPlanMillC m Project
    , MonadPlanMillC m ReportableAssignment 
    , MonadPlanMillC m Task
    , MonadPlanMillC m TimeBalance
    , MonadPlanMillC m Timereport
    , MonadPlanMillC m Team
    , MonadPlanMillC m User
    , MonadPlanMillC m UserCapacity
    , ForallFSymbol (MonadPlanMillC m) EnumDesc
    )
  => MonadPlanMill m where

    -- | Different planmill monads have different constraints
    type MonadPlanMillC m :: * -> Constraint

    -- | We need vector constraints too
    entailMonadPlanMillCVector
        :: Proxy m -> Proxy a
        -> MonadPlanMillC m a :- MonadPlanMillC m (Vector a)

    -- | "Lift" planmill actions to monad action
    planmillAction :: MonadPlanMillC m a => PlanMill a -> m a

-- | Use this for actions retutning @'Vector' a@
planmillVectorAction
    :: forall m a. (MonadPlanMill m, MonadPlanMillC m a)
    => PlanMill (Vector a) -> m (Vector a)
planmillVectorAction = planmillAction \\  -- hello CPP
    entailMonadPlanMillCVector (Proxy :: Proxy m) (Proxy :: Proxy a)

instance ( MonadIO m, MonadHttp m, MonadThrow m, MonadTime m, MonadLogger m
         , Applicative m
         , HasPlanMillBaseUrl env, HasCredentials env
         )
    => MonadPlanMill (ReaderT env m) where
    type MonadPlanMillC (ReaderT env m) = FromJSON
    entailMonadPlanMillCVector _ _ = Sub Dict
    planmillAction planmill = do
        cfg <- undefined
        liftIO $ evalPlanMillIO cfg planmill
