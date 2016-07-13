module Control.Monad.FUM (MonadFUM(..)) where

import Futurice.Prelude
import Prelude          ()

import Data.Aeson.Compat (FromJSON)

import FUM.Request

class (Applicative m, Monad m) => MonadFUM m where
    -- | TODO: unhardcode the constraint
    fumAction :: (FromJSON a, Show a, Typeable a) => FUM a -> m a

