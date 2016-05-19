module Main (main) where

import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM   (TVar, atomically, modifyTVar, newTVarIO,
                                 readTVarIO)
import Data.Time                (NominalDiffTime)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Servant.Cache.Class           as Cache
import qualified Servant.Cache.Internal.DynMap as DynMap

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "cached"
    [ testCase "cold start: broken" coldStartBroken
    , testCase "cold start: fixed" coldStartFixed
    ]

coldStart :: (Cache.DynMapCache -> NominalDiffTime -> Char -> IO () -> IO ())
          -> IO Int
coldStart cached = do
    cache <- DynMap.newIO :: IO Cache.DynMapCache
    ref <- newTVarIO 0 :: IO (TVar Int)
    -- Cache parameters
    let key = 'k'
    let ttl = 1000
    -- Action
    let action = threadDelay 1000 >> atomically (modifyTVar ref (+1))
    -- Cached action, would like to run it only once
    let cachedAction = cached cache ttl key action
    -- Let's run it in parallel
    as <- mapM (const $ async cachedAction) ['a'..'z']
    -- And wait for results
    mapM_ wait as
    -- And see how many times the action is run
    readTVarIO ref

coldStartBroken :: IO ()
coldStartBroken = do
    value <- coldStart Cache.cached
    assertBool ("Should over 13 (close to 26): " ++ show value) (value > 13)

coldStartFixed :: IO ()
coldStartFixed = do
    value <- coldStart Cache.cachedIO
    assertBool ("Less then three: " ++ show value) (value < 3)
