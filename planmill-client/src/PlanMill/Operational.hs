{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
--
module PlanMill.Operational (
    PlanMillT,
    GenPlanMillT,
    PlanMillAction(..),
    runPlanMillT,
    runGenPlanMillT,
    evalPlanMill,
    ) where

import PlanMill.Internal.Prelude
import Prelude                   ()

import Control.Monad             ((<=<))
import Control.Monad.Http        (MonadHttp (..))
import Control.Monad.Logger      (MonadLogger)
import Control.Monad.Operational
import Control.Monad.Reader      (ReaderT (..))
import Control.Monad.Trans.Class (lift)

-- Too long module name
import Control.Monad.CryptoRandom.Extra (GenError, HashDRBG,
                                         MonadInitHashDRBG (..),
                                         evalCRandTThrow)

-- PlanMill import
import PlanMill.Classes
import PlanMill.Eval    (evalPlanMill)
import PlanMill.Types

-- | Free monad with 'PlanMill' actions.
type PlanMillT = GenPlanMillT FromJSON

-- | To be able to perform action, we need to capture dictionary
-- usually 'FromJSON'.
data PlanMillAction k a where
    PlanMillAction :: k a => PlanMill a -> PlanMillAction k a
  deriving (Typeable)

deriving instance Eq (PlanMillAction k a)
deriving instance Ord (PlanMillAction k a)
deriving instance Show (PlanMillAction k a)

instance Hashable (PlanMillAction k a) where
    hashWithSalt salt (PlanMillAction x) = hashWithSalt salt x

type GenPlanMillT k = ProgramT (PlanMillAction k)

-- | Lower 'PlanMillT'.
runPlanMillT
    :: forall m env a.
        ( MonadInitHashDRBG m, MonadHttp m, MonadLogger m
        , MonadThrow m, MonadTime m
        , Applicative m
        , MonadReader env m, HasPlanMillBaseUrl env, HasCredentials env
        )
    => PlanMillT m a -> m a
runPlanMillT action = do
    cfg <- askCfg
    g <- mkHashDRBG
    let action' = runPlanMillT' action
    evalCRandTThrow (runReaderT  action' cfg) g

runPlanMillT'
    :: forall m a.
        ( MonadHttp m, MonadThrow m, MonadTime m, MonadLogger m
        , Applicative m
        )
    => PlanMillT m a
    -> ReaderT Cfg (CRandT HashDRBG GenError m) a
runPlanMillT' = runGenPlanMillT evalPlanMill (lift . lift)

--  | General lowering of 'GenPlanMillT'.
runGenPlanMillT
    :: forall m n k a. (Applicative m, Monad m, Applicative n, Monad n)
    => (forall b. k b => PlanMill b -> n b)      -- ^ Method to perform actual planmill request
    -> (forall c. m c -> n c)                    -- ^ Natural transformation into result monad
    -> GenPlanMillT k m a                        -- ^ General action
    -> n a
runGenPlanMillT perform nt = eval <=< nt . viewT
  where
    eval :: ProgramViewT (PlanMillAction k) m a -> n a
    eval (Return x) = pure x
    eval (PlanMillAction planmill :>>= k) = do
        res <- perform planmill
        runGenPlanMillT perform nt (k res)
