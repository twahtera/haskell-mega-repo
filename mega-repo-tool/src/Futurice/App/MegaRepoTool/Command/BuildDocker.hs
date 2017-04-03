{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.MegaRepoTool.Command.BuildDocker (
    buildDocker,
    AppName,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Aeson       (FromJSON (..), withObject, (.:))
import Data.Yaml        (decodeFileEither)
import System.Exit      (exitFailure)
import System.IO        (hClose, hFlush)
import System.IO.Temp   (withTempFile)
import System.Process   (callProcess, readProcess)

import qualified Data.Map     as Map
import qualified Data.Text    as T
import qualified Data.Text.IO as T

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type AppName = Text

data ImageDefinition = ImageDefinition
    { _idDockerImage :: !Text
    , _idExecutable  :: !Text
    }
  deriving (Show)

instance FromJSON ImageDefinition where
    parseJSON = withObject "ImageDefinition" $ \obj -> ImageDefinition
        <$> obj .: "docker"
        <*> obj .: "executable"

data MRTConfig = MRTConfig
    { mrtDockerBaseImage :: !Text
    , _mrtApps            :: !(Map AppName ImageDefinition)
    }
  deriving (Show)

makeLenses ''MRTConfig

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

buildDocker :: [AppName] -> IO ()
buildDocker appnames = do
    -- Read config
    cfg <- either throwM pure =<< decodeFileEither "mega-repo-tool.yaml"

    -- `some` verifies images aren't empty
    when (null appnames) $ do
        putStrLn "Image names are required"
        exitFailure

    -- What apps to build?
    apps <- fmap Map.fromList $ for appnames $ \appname -> do
        case cfg ^? mrtApps . ix appname of
            Nothing -> do
                putStrLn $ "Unknown app: " <> appname ^. unpacked
                exitFailure
            Just app -> pure (appname, app)

    -- Get the hash of current commit
    githash' <- readProcess "git" ["log", "--pretty=format:%h", "-n", "1"] ""
    _        <- readProcess "git" ["rev-parse", "--verify", githash'] ""
    let githash = githash' ^. packed
    T.putStrLn $ "Git hash aka tag for images: " <> githash

    -- Check that binaries are build with current hash
    githashBuild <- (T.strip <$> T.readFile "build/git-hash.txt") `catch` onIOError "<none>"

    when (githashBuild /= githash) $ do
        T.putStrLn $ "Git hash in build directory don't match: " <> githashBuild  <> " != " <> githash
        T.putStrLn $ "Make sure you have data volumes:"
        T.putStrLn $ "  docker volume create --name hmr-stack-root"
        T.putStrLn $ "  docker volume create --name hmr-stack-work"
        T.putStrLn $ "Run following command to build image:"
        T.putStrLn $ "  " <> (buildCmd $ mrtDockerBaseImage cfg)
        exitFailure

    -- Build docker images
    images <- ifor apps $ \appname (ImageDefinition image exe) -> do
        -- Write Dockerfile
        let dockerfile' = dockerfile exe
        let directory = "build/" <> exe ^. unpacked
        withTempFile directory "Dockerfile." $ \fp handle -> do
            T.hPutStrLn handle dockerfile'
            hFlush handle
            hClose handle

            -- Build an image
            let fullimage = "futurice/" <> image <> ":" <> githash
            callProcess "docker" ["build", "-t", T.unpack fullimage, "-f", fp, directory]

            -- accumulate image names
            pure (appname, fullimage, image)

    T.putStrLn "Upload images by:"
    for_ images $ \(_, image, _) ->
        T.putStrLn $ "  docker push " <> image

    T.putStrLn "Deploy iamges by:"
    for_ images $ \(appname, _, image) ->
        T.putStrLn $ "  futuswarm app:deploy"
            <> " --name " <> appname
            <> " --image " <> image
            <> " --tag " <> githash

dockerfile :: Text -> Text
dockerfile exe = T.unlines $
    [ "FROM ubuntu:trusty"
    , "MAINTAINER Oleg Grenrus <oleg.grenrus@iki.fi>"
    , "RUN apt-get -yq update && apt-get -yq --no-install-suggests --no-install-recommends --force-yes install " <> T.intercalate " " debs <> " && rm -rf /var/lib/apt/lists/*"
    , "RUN useradd -m -s /bin/bash -d /app app"
    , "EXPOSE 8000"
    , "WORKDIR /app"
    , "ADD " <> exe <> " /app"
    , "RUN chown -R app:app /app"
    , "USER app"
    , "CMD [\"/app/" <> exe <> "\", \"+RTS\", \"-N4\", \"-A32m\", \"-T\"]"
    ]
  where
    debs =
        [ "ca-certificates"
        , "libfftw3-bin"
        , "libgmp10"
        , "libpq5"
        , "curl"
        ]

onIOError :: Monad m => a -> IOError -> m a
onIOError v _ = return v
