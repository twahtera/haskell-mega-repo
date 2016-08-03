{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-- | A @haxl@ datasource based on 'Query'
--
-- The instance for 'DataSource' is explicit. As we reuse 'Query' as
-- 'DataSource', we pass actual 'fetch' implementation in a 'State'
module PlanMill.Queries.Haxl where

import PlanMill.Internal.Prelude

import Haxl.Core

import PlanMill.Types.Cfg   (Cfg)
import PlanMill.Types.Query

-- For initDataSourceSimpleIO
import Control.Monad.CryptoRandom.Extra
       (MonadInitHashDRBG (..), evalCRandTThrow)
import Control.Monad.Http               (evalHttpT)
import Control.Monad.Reader             (runReaderT)
import Data.Constraint                  (Dict (..), type (:-)(..))
import PlanMill.Eval                    (evalPlanMill)

instance Show1 Query where show1 = show

instance StateKey Query where
    data State Query = QueryFunction ([BlockedFetch Query] -> PerformFetch)

instance DataSourceName Query where
    dataSourceName _ = "Planmill.Query"

instance DataSource u Query where
    fetch (QueryFunction f) _flags _userEnv = f

-- | This is a simple query function.
--
-- It's smart enough to reuse http/random-gen for blocked fetches
initDataSourceSimpleIO :: Cfg -> State Query
initDataSourceSimpleIO cfg = QueryFunction $ \blockedFetches -> SyncFetch $ do
    g <- mkHashDRBG
    perform g $ for_ blockedFetches $ \(BlockedFetch q v) ->
        case queryDict (Proxy :: Proxy FromJSON) (Sub Dict) q of
            Dict -> do
                res <- evalPlanMill $ queryToRequest q
                liftIO $ putSuccess v res
  where
    perform g = evalHttpT . runStderrLoggingT . flip runReaderT cfg . flip evalCRandTThrow g
