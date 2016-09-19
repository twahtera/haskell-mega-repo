{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif
module Control.Monad.FUM (MonadFUM(..)) where

import Futurice.Prelude
import Prelude ()

import Data.Constraint (Constraint)

import Data.Aeson.Compat (FromJSON)

import FUM.Request
import FUM.Types   (User)

-- | Class of monads which can perform FUM actions
class
    ( Monad m
    , MonadFUMC m (Vector User)
    )
    => MonadFUM m
  where
    type MonadFUMC m a :: Constraint
    type MonadFUMC m a = FromJSON a

    fumAction :: (Show a, Typeable a, MonadFUMC m a) => FUM a -> m a

