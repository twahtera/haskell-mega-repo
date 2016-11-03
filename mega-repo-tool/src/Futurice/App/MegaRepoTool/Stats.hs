{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.MegaRepoTool.Stats (stats) where

import Prelude ()
import Futurice.Prelude
import Data.List                (isSuffixOf)
import Data.Machine
import Data.Machine.Runner      (foldT)
import System.Directory.Machine (files, directoryWalk')

import qualified Data.Text    as T
import qualified Data.Text.IO as T

-------------------------------------------------------------------------------
-- Stats monoid
-------------------------------------------------------------------------------

-- | TODO: collect other stats as well
data Stats = Stats
    { _statsFiles   :: !Int
    , _statsLines   :: !Int
    , _statsNELines :: !Int
    }
    deriving Show

makeLenses ''Stats

instance Semigroup Stats where
    Stats ax ay az <> Stats bx by bz = Stats
        (ax + bx)
        (ay + by)
        (az + bz)

instance Monoid Stats where
    mempty = Stats 0 0 0
    mappend = (<>)

lineStats :: Text -> Stats
lineStats t = Stats 0 1 ne
  where
    ne | isn't _Empty t = 1
       | otherwise      = 0

-------------------------------------------------------------------------------
-- Machine
-------------------------------------------------------------------------------

statsMachine :: ProcessT IO FilePath Stats
statsMachine
    =  directoryWalk' dirPredicate
    ~> files
    ~> filtered (isSuffixOf ".hs")
    ~> autoM (fmap T.lines . T.readFile)
    ~> mapping ((statsFiles .~ 1) . foldMap lineStats)
  where
    dirPredicate d = not . any ($ d) $
        [ isSuffixOf ".git"
        , isSuffixOf ".stack-work"
        -- build-in-docker.sh artifacts
        , isSuffixOf ".stack-root"
        , isSuffixOf ".stack-work-docker"
        ]

stats :: IO ()
stats = do 
    Stats fs ls nels <- foldT (statsMachine <~ source ["."])
    putStrLn $ "total files:     " ++ show fs
    putStrLn $ "total lines:     " ++ show ls
    putStrLn $ "non-empty lines: " ++ show nels


