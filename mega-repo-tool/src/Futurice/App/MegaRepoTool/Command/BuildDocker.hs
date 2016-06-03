{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.MegaRepoTool.Command.BuildDocker (
    buildDocker
    ) where

import Futurice.Prelude
import Turtle           hiding ((<>))

import Data.Foldable (for_)
import System.IO     (hClose, hFlush)

import qualified Control.Foldl as Fold
import qualified Data.Text     as T
import qualified Data.Text.IO  as T

type Image = Text
type Executable = Text

apps :: [(Image, Executable)]
apps =
    [ ("avatar", "avatar-server")
    , ("contacts-api", "contacts-server")
    , ("favicon", "favicon")
    , ("spice-stats", "spice-stats-server")
    , ("futuhours-api", "futuhours-api-server")
    , ("github-dashboard", "github-dashboard-server")
    , ("proxy-app", "proxy-app-server")
    ]

buildImage :: Text
buildImage = "futurice/base-images:haskell-lts-6.0-1"

buildCmd :: Text
buildCmd = T.unwords
    [ "docker run"
    , "--rm"
    , "--entrypoint /app/src/build-in-docker.sh"
    , "-v $(pwd):/app/src"
    , "-v $(pwd)/build:/app/bin"
    , buildImage
    ]

buildDocker :: IO ()
buildDocker = do
    -- Build binaries inside the docker
    shells buildCmd mempty

    -- Get the hash of current commit
    githash <- fold (inshell "git log --pretty=format:'%h' -n 1" empty) $ Fold.lastDef "HEAD"
    shells ("git rev-parse --verify " <> githash) empty

    print githash

    for_ apps $ \(image, exe) -> sh $ do
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

dockerfile :: Executable -> Text
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
