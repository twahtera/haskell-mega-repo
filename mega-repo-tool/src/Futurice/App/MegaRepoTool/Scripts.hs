{-# LANGUAGE OverloadedStrings  #-}
module Futurice.App.MegaRepoTool.Scripts (
    dotScript,
    packdepsScript,
    ) where

import Futurice.Prelude hiding (foldMap, fold)
import Prelude ()

import Turtle

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
