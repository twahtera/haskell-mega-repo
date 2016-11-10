{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.MegaRepoTool.Command.BuildDocker (
    buildDocker,
    ImageName,
    ) where

import Data.Aeson       (FromJSON (..), withObject, (.:))
import Data.Yaml        (decodeFileEither)
import Futurice.Prelude hiding (fold)
import Prelude ()
import Turtle           hiding (when, (<>))

import System.Exit (exitFailure)
import System.IO   (hClose, hFlush)

import qualified Control.Foldl as Fold
import qualified Data.Map      as Map
import qualified Data.Text     as T
import qualified Data.Text.IO  as T

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type ImageName = Text

newtype ImageDefinition = ImageDefinition Text
  deriving (Show)

instance FromJSON ImageDefinition where
    parseJSON = withObject "ImageDefinition" $ \obj -> ImageDefinition
        <$> obj .: "executable"

data MRTConfig = MRTConfig
    { mrtDockerBaseImage :: !Text
    , mrtApps            :: !(Map ImageName ImageDefinition)
    }
  deriving (Show)

instance FromJSON MRTConfig where
    parseJSON = withObject "MRTConfig" $ \obj -> MRTConfig
        <$> obj .: "docker-base-image"
        <*> obj .: "apps"

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

buildCmd :: Text -> Text
buildCmd buildImage = T.unwords
    [ "docker run"
    , "--rm"
    , "-ti"
    , "--entrypoint /app/src/build-in-docker.sh"
    , "-v $(pwd):/app/src"
    , "-v $(pwd)/build:/app/bin"
    , "-v hmr-stack-root:/stack-root"
    , "-v hmr-stack-work:/app/src/.stack-work-docker"
    , buildImage
    ]

-------------------------------------------------------------------------------
-- Command

buildDocker :: [ImageName] -> IO ()
buildDocker imgs = do
    -- Read config
    cfg <- either throwM pure =<< decodeFileEither ".mega-repo-tool.yaml"

    -- What apps to build?
    let apps | null imgs = mrtApps cfg
             | otherwise = Map.filterWithKey (\k _ -> k `elem` imgs) $ mrtApps cfg

    -- Get the hash of current commit
    githash <- fold (inshell "git log --pretty=format:'%h' -n 1" empty) $ Fold.lastDef "HEAD"
    shells ("git rev-parse --verify " <> githash) empty
    T.putStrLn $ "Git hash aka tag for images: " <> githash

    -- Check that binaries are build with current hash
    githashBuild <- (T.strip <$> T.readFile "build/git-hash.txt") `catch` onIOError "<none>"
    when (githashBuild /= githash) $ do
        T.putStrLn $ "Git hash in build directory don't match: " <> githashBuild  <> " != " <> githash
        T.putStrLn $ "Make sure you have data volumes: "
        T.putStrLn $ "  docker volume create --name hmr-stack-root"
        T.putStrLn $ "  docker volume create --name hmr-stack-work"
        T.putStrLn $ "Run following command to build image:"
        T.putStrLn $ "  " <> (buildCmd $ mrtDockerBaseImage cfg)
        exitFailure

    -- Build docker images
    ifor_ apps $ \image (ImageDefinition exe) -> sh $ do
        -- Write Dockerfile
        let dockerfile' = dockerfile exe
        (file,handle) <- using $ mktemp "build" "Dockerfile"
        liftIO $ do
            T.hPutStrLn handle dockerfile'
            hFlush handle
            hClose handle

        -- Build an image
        let fullimage = "futurice/" <> image <> ":" <> githash
        procs "docker" ["build", "-t", fullimage, "-f", format fp file, "build" ] empty

dockerfile :: Text -> Text
dockerfile exe = T.unlines $
    [ "FROM ubuntu:trusty"
    , "MAINTAINER Oleg Grenrus <oleg.grenrus@iki.fi>"
    , "RUN apt-get -yq update && apt-get -yq --no-install-suggests --no-install-recommends --force-yes install " <> T.intercalate " " debs <> " && rm -rf /var/lib/apt/lists/*"
    , "RUN useradd -m -s /bin/bash -d /app app"
    , "WORKDIR /app"
    , "ADD " <> exe <> " /app"
    , "RUN chown -R app:app /app"
    , "EXPOSE 8000"
    , "USER app"
    , "WORKDIR /app"
    , "CMD [\"/app/" <> exe <> "\", \"+RTS\", \"-N\", \"-T\"]"
    ]
  where
    debs =
        [ "ca-certificates"
        , "libfftw3-bin"
        , "libgmp10"
        , "libpq5"
        ]

onIOError :: Monad m => a -> IOError -> m a
onIOError v _ = return v
