module Control.Monad.FUM (MonadFUM(..)) where

import Futurice.Prelude
import Prelude          ()

import Data.Aeson.Compat (FromJSON)

import FUM.Request

class (Applicative m, Monad m) => MonadFUM m where
    fumAction :: FromJSON a => FUM a -> m a

