{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
module Data.TDigest.Metrics
    ( MonadMetrics (..)
    , registerTDigest
    )
    where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM (atomically)

import qualified Data.TDigest                         as TD
import qualified Focus
import qualified STMContainers.Map                    as StmMap
import qualified System.Metrics                       as Ekg
import qualified System.Metrics.Distribution.Internal as Ekg

import System.IO.Unsafe (unsafePerformIO)

class Monad m => MonadMetrics m where
    writeMetric
        :: Text    -- ^ Key
        -> Double  -- ^ Gauge value
        -> m ()

-- | Generic, overlapping instance.
instance {-# OVERLAPPABLE #-}
    ( MonadMetrics m
    , MonadTrans t
    , Monad (t m)
    ) => MonadMetrics (t m)
  where
    writeMetric k = lift . writeMetric k

-- | Base instance for 'IO', which uses global store.
instance MonadMetrics IO where
    writeMetric k v = atomically $ do
        StmMap.focus s k globalMap
      where
        s Nothing   = pure $ ((), Focus.Replace $ TD.singleton v)
        s (Just td) = pure $ ((), Focus.Replace $ TD.insert v td)

-- | Register 'TDigest' from global t-digest store to 'Ekg.Store'.
registerTDigest
    :: Text       -- ^ Digest name
    -> [Double]   -- ^ Quantiles
    -> Ekg.Store  -- ^ Metrics store
    -> IO ()
registerTDigest name qs store = for_ qs $ \q ->
    Ekg.registerDistribution
        (name <> ".q" <> textShow (intQ q))
        (action q)
        store
  where
    intQ :: Double -> Int
    intQ q = truncate (q * 1000)

    action q = atomically $ do
        td <- StmMap.lookup name globalMap
        pure $ mkStats q $ fromMaybe mempty td

    mkStats q td = Ekg.Stats
        { Ekg.mean     = fromMaybe 0.0 $ TD.quantile q td
        , Ekg.variance = 0
        , Ekg.sum      = 0
        , Ekg.count    = truncate (TD.totalWeight td)
        , Ekg.min      = TD.minimumValue td
        , Ekg.max      = TD.maximumValue td
        }

-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------

globalMap :: StmMap.Map Text (TD.TDigest 50)
globalMap = unsafePerformIO StmMap.newIO
