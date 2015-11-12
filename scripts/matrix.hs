#!/usr/bin/env stack
-- stack runghc --package process

import Control.Monad (forM_)
import Data.Char (isSpace)
import System.Process (readProcess)

-- Saw full green matrix!
greens :: [String]
greens =
    [ "base"
    , "JuicyPixels-util"
    , "MonadRandom"
    , "QuickCheck"
    , "SHA"
    , "StateVar"
    , "aeson"
    , "aeson-extra"
    , "aeson-pretty"
    , "ansi-pretty"
    , "ansi-terminal"
    , "ansi-wl-pprint"
    , "ansi-wl-terminal"
    , "appar"
    , "array"
    , "asn1-encoding"
    , "asn1-parse"
    , "asn1-types"
    , "async"
    , "attoparsec"
    , "auto-update"
    , "base-compat"
    , "base-prelude"
    , "base64-bytestring"
    , "bifunctors"
    , "binary"
    , "binary-orphans"
    , "binary-tagged"
    , "blaze-builder"
    , "blaze-markup"
    , "blaze-textual"
    , "byteable"
    , "bytestring"
    , "case-insensitive"
    , "cereal"
    , "cipher-aes"
    , "cipher-des"
    , "comonad"
    , "conduit"
    , "containers"
    , "contravariant"
    ]

main :: IO ()
main = do
    deps <- readProcess "stack" ["list-dependencies"] ""
    let pkgs = filter (`notElem` greens) . map (takeWhile (not . isSpace)) $ lines deps
    forM_ (take 10 pkgs) $ \pkg -> do
        putStrLn $ "sleep 1; open 'http://matrix.hackage.haskell.org/package/" ++ pkg ++ "'"
