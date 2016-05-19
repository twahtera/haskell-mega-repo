{-# LANGUAGE OverloadedStrings #-}
module Servant.Futurice.Status.Uptime (
    uptimeStatusInfo,
    uptimeStatusInfo',
    ) where

import Prelude        ()
import Prelude.Compat

import Data.Time (UTCTime, diffUTCTime, getCurrentTime)

import qualified Data.Text as T

import Servant.Futurice.Status.Types

uptimeStatusInfo' :: UTCTime -> UTCTime -> StatusInfo
uptimeStatusInfo' start now =
    info "uptime" $ T.pack $ show $ now `diffUTCTime` start

uptimeStatusInfo :: UTCTime -> StatusInfoIO
uptimeStatusInfo start = SIIO $ uptimeStatusInfo' start <$> getCurrentTime
