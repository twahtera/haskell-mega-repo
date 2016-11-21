{-# LANGUAGE OverloadedStrings #-}
module Log.Backend.Papertrail (
    -- * Logger
    withPapertrailLogger,
    ) where

import Prelude ()
import Prelude.Compat

-- | TODO: intergrate with @log@
type Logger = ()

withPapertrailLogger :: (Logger -> IO r) -> IO r
withPapertrailLogger f = f ()
