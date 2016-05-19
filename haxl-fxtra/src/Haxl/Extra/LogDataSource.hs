{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

-- Author: Simon Marlow
module Haxl.Extra.LogDataSource
    ( writeLog
    , printLog
    , initDataSource
    , LogRequest
    ) where

import           Data.Hashable           (Hashable)
import           Data.Typeable           (Typeable)
import           GHC.Generics            (Generic)
import           Haxl.Core
import qualified Haxl.Extra.IODataSource as IODS

newtype LogTag = LogTag String
    deriving (Eq, Show, Generic, Typeable)

instance Hashable LogTag

type LogRequest = IODS.GenIORequest LogTag

instance IODS.IODataSourceTag LogTag where
    type IOEnv LogTag = ()
    ioSourceName _ = "LogDataSource"

writeLog :: String -> GenHaxl u ()
writeLog s = IODS.ioAction (LogTag s) (\_ -> putStrLn s)

printLog :: Show a => a -> GenHaxl u ()
printLog = writeLog . show

initDataSource :: IO (State LogRequest)
initDataSource = IODS.initDataSource 1 ()
