{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futurice.App.HoursApi.Monad (
    Hours,
    ) where

import Futurice.Prelude
import Prelude ()
import Servant (Handler)

import Futurice.App.HoursApi.Class

-- | A "real" implementation of 'MonadHours'.
--
-- /TODO:/ :)
newtype Hours a = Hours { _unHours :: ReaderT Env (LogT Handler) a }
  deriving (Functor, Applicative, Monad)

data Env = Env
