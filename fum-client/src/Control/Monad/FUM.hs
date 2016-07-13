{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies    #-}
module Control.Monad.FUM (MonadFUM(..)) where

import Futurice.Prelude
import Prelude          ()

import Data.Constraint (Constraint)

import Data.Aeson.Compat (FromJSON)

import FUM.Request

-- | Class of monads which can perform FUM actions
class (Applicative m, Monad m) => MonadFUM m where
    type MonadFUMC m a :: Constraint
    type MonadFUMC m a = FromJSON a

    fumAction :: MonadFUMC m a => FUM a -> m a

