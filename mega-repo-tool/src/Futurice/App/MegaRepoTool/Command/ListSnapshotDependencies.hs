{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.MegaRepoTool.Command.ListSnapshotDependencies (
    listSnapshotDependencies
    ) where

import Futurice.Prelude

import Data.Aeson.Compat       (FromJSON (..), decode, withObject, (.:))
import Data.Char               (isSpace)
import Data.Maybe              (mapMaybe)
import Network.HTTP.Client     (httpLbs, newManager, parseUrl, requestHeaders,
                                responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Process          (readProcess)

import qualified Data.Set     as Set
import qualified Data.Text.IO as T

import Text.Regex.Applicative (anySym, match, psym)

data Pkg = Pkg
    { pkgName     :: !Text
    , _pkgVersion :: !Text
    }
    deriving (Eq, Ord, Show)

instance FromJSON Pkg where
    parseJSON = withObject "Pkg" $ \obj -> Pkg
        <$> obj .: "name"
        <*> obj .: "version"

stackListDependencies :: IO (Set Pkg)
stackListDependencies = f <$> readProcess "stack" ["list-dependencies"] ""
  where
    f = Set.fromList . mapMaybe (match re) . lines
    re = pkg
        <$> some (psym (not . isSpace))
        <*  some (psym isSpace)
        <*> many anySym
    pkg n v = Pkg (n ^. packed) (v ^. packed)

newtype Snapshot = Snapshot { getSnapshotPkgs :: Set Pkg }

instance FromJSON Snapshot where
    parseJSON = withObject "Snapshot" $ \obj -> Snapshot
        <$> obj .: "packages"

listSnapshotDependencies :: IO ()
listSnapshotDependencies = do
    deps <- stackListDependencies
    req' <- parseUrl $ "https://www.stackage.org/" <> snapshotName
    let req = req' { requestHeaders = acceptJson : requestHeaders req' }
    mgr <- newManager tlsManagerSettings
    snapshotBs <- responseBody <$> httpLbs req mgr
    snapshot <- getSnapshotPkgs <$> decode snapshotBs
    let inSnapshot = Set.intersection deps snapshot
    traverse_ (T.putStrLn . pad . pkgName) inSnapshot
 where
    pad s = "  " <> s <> " \\"
    acceptJson = ("Accept", "application/json")

snapshotName :: String
snapshotName = "lts-6.0"
