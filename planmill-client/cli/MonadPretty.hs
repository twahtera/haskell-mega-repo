{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module MonadPretty (MonadPretty(..)) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Monoid            ((<>))

import Text.PrettyPrint.ANSI.Leijen.AnsiPretty (AnsiPretty (..), linebreak,
                                                putDoc)

class MonadPretty m where
    putPretty :: AnsiPretty a => a -> m ()

instance MonadIO m => MonadPretty m where
    putPretty = liftIO . putDoc . (<> linebreak) . ansiPretty
