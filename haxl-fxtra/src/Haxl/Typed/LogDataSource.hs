{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Haxl.Typed.LogDataSource (
    writeLog,
    printLog,
    Haxl.initDataSource,
    Haxl.LogRequest,
    ) where

import Futurice.Prelude

import qualified Haxl.Extra.LogDataSource as Haxl
import           Haxl.Typed

writeLog
    :: IsElem' Haxl.LogRequest r (Index Haxl.LogRequest r)
    => String
    -> GenTyHaxl r u ()
writeLog = GenTyHaxl . Haxl.writeLog

printLog
    :: (Show a, IsElem' Haxl.LogRequest r (Index Haxl.LogRequest r))
    => a
    -> GenTyHaxl r u ()
printLog = GenTyHaxl . Haxl.printLog
