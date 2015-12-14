#!/usr/bin/env stack
-- stack runghc --package process

import Control.Applicative
import Control.Monad (forM_)
import Data.Char (isSpace)
import System.Process (readProcess)
import Prelude

main :: IO ()
main = do
    greens <- map (head . words) . filter ((/='#') . head) . filter (not . null) . lines <$> readFile "scripts/matrix-green.txt"
    deps <- readProcess "stack" ["list-dependencies"] ""
    let pkgs = filter (`notElem` greens) . map (takeWhile (not . isSpace)) $ lines deps
    forM_ (take 10 pkgs) $ \pkg -> do
        putStrLn $ "sleep 1; open 'http://matrix.hackage.haskell.org/package/" ++ pkg ++ "'"
