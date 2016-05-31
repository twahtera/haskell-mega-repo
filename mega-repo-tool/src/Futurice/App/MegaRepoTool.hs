{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NoImplicitPrelude  #-}
module Futurice.App.MegaRepoTool (defaultMain) where

import Futurice.Prelude

import qualified Options.Applicative as O

import Futurice.App.MegaRepoTool.Command.ListSnapshotDependencies

data Cmd
    = ListSnapshotDependencies 
    deriving Show

listSnapshotDependenciesOptions :: O.Parser Cmd
listSnapshotDependenciesOptions = pure ListSnapshotDependencies

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "list-snapshot-dependencies" listSnapshotDependenciesOptions "List snapshot dependencies (like stack list-dependencies)"
    ]
  where
    cmdParser :: String -> O.Parser Cmd -> String -> O.Mod O.CommandFields Cmd
    cmdParser cmd parser desc =
         O.command cmd $ O.info (O.helper <*> parser) $ O.progDesc desc

main' :: Cmd -> IO ()
main' ListSnapshotDependencies = listSnapshotDependencies

defaultMain :: IO ()
defaultMain = do
    O.execParser opts >>= main'
  where
    opts = O.info (O.helper <*> optsParser) $ mconcat
        [ O.fullDesc
        , O.progDesc "Futurice haskell-mega-repo tool"
        , O.header "mega-repo-tool"
        ]
