{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.MegaRepoTool.Stats (
    stats,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.List                (isSuffixOf)
import Data.Machine
import Data.Machine.Runner      (foldT)
import Data.Maybe               (fromJust)
import Data.Semigroup           (Max (..))
import Data.Semigroup.Generic   (gmappend, gmempty)
import Data.TDigest             (TDigest, quantile, singleton)
import Futurice.Monoid          (Average (..))
import System.Directory.Machine (directoryWalk', files)
import Text.Printf              (printf)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

-------------------------------------------------------------------------------
-- Stats monoid
-------------------------------------------------------------------------------

-- | TODO: collect other stats as well
data Stats = Stats
    { _statsFiles    :: !(Sum Int)
    , _statsLines    :: !(Sum Int)
    , _statsNELines  :: !(Sum Int)
    , _statsLinesAvg :: !(Average Double)
    , _statsLinesMax :: !(Max Int)
    , _statsLinesTD  :: !(TDigest 100)
    }
    deriving (Show, Generic)

instance Semigroup Stats where
    (<>) = gmappend

instance Monoid Stats where
    mempty = gmempty
    mappend = (<>)

fileStats :: Text -> Stats
fileStats t = Stats
    { _statsFiles    = Sum $ 1
    , _statsLines    = Sum $ lineCount
    , _statsNELines  = Sum $ length . filter (not . T.null) $ ls
    , _statsLinesAvg = Average 1 $ fromIntegral lineCount
    , _statsLinesMax = Max $ lineCount
    , _statsLinesTD  = singleton $ fromIntegral lineCount
    }
  where
    ls        = T.lines t
    lineCount = length ls

-------------------------------------------------------------------------------
-- Machine
-------------------------------------------------------------------------------

statsMachine :: ProcessT IO FilePath Stats
statsMachine
    =  directoryWalk' dirPredicate
    ~> files
    ~> filtered filePredicate
    ~> autoM T.readFile
    ~> mapping fileStats
  where
    dirPredicate d = not . any ($ d) $
        [ isSuffixOf ".git"
        , isSuffixOf ".stack-work"
        , isSuffixOf ".stack-work-dev"
        -- build-in-docker.sh artifacts
        , isSuffixOf ".stack-root"
        , isSuffixOf ".stack-work-docker"
        -- some code we just have around
        , isSuffixOf "deprecated"
        ]

    filePredicate f = isSuffixOf ".hs" f
        && not (isSuffixOf "Setup.hs" f)
        && not (isSuffixOf "Main.hs" f)

stats :: IO ()
stats = do
    Stats fs ls nels lavg lmax td <- foldT (statsMachine <~ source ["."])
    putStrLn $ "total files:     " ++ show (getSum fs)
    putStrLn $ "total lines:     " ++ show (getSum ls)
    putStrLn $ "non-empty lines: " ++ show (getSum nels)
    putStrLn $ "average lines:   " ++ printf "%2.2f" (getAverage lavg)
    putStrLn $ "line count 10%:  " ++ printf "%2.2f" (fromJust $ quantile 0.1 td)
    putStrLn $ "line count 25%:  " ++ printf "%2.2f" (fromJust $ quantile 0.25 td)
    putStrLn $ "line count 50%:  " ++ printf "%2.2f" (fromJust $ quantile 0.5 td)
    putStrLn $ "line count 75%:  " ++ printf "%2.2f" (fromJust $ quantile 0.76 td)
    putStrLn $ "line count 90%:  " ++ printf "%2.2f" (fromJust $ quantile 0.9 td)
    putStrLn $ "line count 95%:  " ++ printf "%2.2f" (fromJust $ quantile 0.95 td)
    putStrLn $ "line count 98%:  " ++ printf "%2.2f" (fromJust $ quantile 0.98 td)
    putStrLn $ "max lines:       " ++ show (getMax lmax)
