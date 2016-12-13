module Main (main) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM
       (TVar, atomically, modifyTVar, newTVarIO, readTVarIO)

import Test.Tasty
import Test.Tasty.HUnit

import qualified Futurice.Cache  as Cache
import qualified Futurice.DynMap as DynMap

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "cached"
    [ testCase "cold start: broken" coldStartBroken
    , testCase "cold start: fixed" coldStartFixed
    , testCase "DynMap: same key, different value types" dynmapExample
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
coldStartFixed = withStderrLogger $ \logger -> do
    value <- coldStart $ Cache.cachedIO logger
    assertBool ("Less then three: " ++ show value) (value < 3)

dynmapExample :: IO ()
dynmapExample = do
    dm <- DynMap.newIO
    -- Insert
    atomically $ DynMap.insert u (I 1 :: I Int) dm
    atomically $ DynMap.insert u (I 2 :: I Double) dm
    -- Lookup
    mi <- atomically $ DynMap.lookup u Proxy dm
    md <- atomically $ DynMap.lookup u Proxy dm
    -- Test
    mi @?= Just (I 1 :: I Int)
    md @?= Just (I 2 :: I Double)
  where
    u = I ()
