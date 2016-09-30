{-# LANGUAGE OverloadedStrings  #-}
module Futurice.App.MegaRepoTool.Scripts (
    dotScript,
    packdepsScript,
    statsScript,
    ) where

import Futurice.Prelude hiding (foldMap, fold)
import Prelude ()

import Control.Foldl (foldMap)
import Turtle hiding ((<>))

import qualified Data.Text as T

find' :: (Text -> Bool) -> Turtle.FilePath -> Shell Turtle.FilePath
find' p path = mfilter p' $ lsif predicate path
  where
    p' = p . format fp
    predicate = pure . (/= ".stack-work")

-- |
-- @
-- packdeps "**/*.cabal"
-- @
packdepsScript :: Shell Text
packdepsScript
    = inshell "xargs packdeps"
    $ format fp <$> find' predicate "."
  where
    predicate :: Text -> Bool
    predicate path = not (".stack-work" `T.isInfixOf` path) && ".cabal" `T.isSuffixOf` path

dotScript :: Shell Text
dotScript
    = inshell "dot -Tpng -o deps.png"
    $ inshell "tred"
    $ inshell "stack dot"
    $ mempty

-- | TODO: collect other stats as well
statsScript :: Shell Text
statsScript =
    textShow <$> fold allLines (foldMap lineStats id)
  where
    allLines :: Shell Text
    allLines = do
        files <- find' predicate "."
        input files
    predicate :: Text -> Bool
    predicate path = not (".stack-work" `T.isInfixOf` path) && ".hs" `T.isSuffixOf` path

data Stats = Stats
    { _statsLines :: !Int
    , _statsNELines :: !Int
    }
    deriving Show

instance Semigroup Stats where
    Stats ax ay <> Stats bx by = Stats
        (ax + bx)
        (ay + by)

instance Monoid Stats where
    mempty = Stats 0 0
    mappend = (<>)

lineStats :: Text -> Stats
lineStats t = Stats 1 ne
  where
    ne | isn't _Empty t = 1
       | otherwise      = 0
