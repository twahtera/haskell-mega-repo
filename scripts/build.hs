#!/usr/bin/env stack
-- stack --resolver=lts-5.15 runghc --package turtle --package text
{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Data.Foldable (for_)
import System.IO (hClose, hFlush)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Foldl as Fold

type Image = Text
type Executable = Text

apps :: [(Image, Executable)]
apps =
    [ ("avatar", "avatar-server")
    , ("contacts-api", "contacts-server")
    , ("favicon", "favicon")
    , ("spice-stats", "spice-stats-server")
    , ("futuhours-api", "futuhours-api-server")
    ]

main :: IO ()
main = do
    -- Build binaries inside the docker
    -- shells ("docker run --entrypoint /app/src/scripts/build-in-docker.sh --rm -v $(pwd):/app/src -v $(pwd)/build:/app/bin futurice/base-images:haskell-lts-5.15-1") empty

    -- Get the hash of current commit
    githash <- fold (inshell "git log --pretty=format:'%h' -n 1" empty) $ Fold.lastDef "HEAD"
    shells ("git rev-parse --verify " <> githash) empty

    print githash

    for_ apps $ \(image, executable) -> sh $ do
        -- Write Dockerfile
        let d = dockerfile executable
        (file,handle) <- using $ mktemp "build" "Dockerfile"
        liftIO $ do
            T.hPutStrLn handle d
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
