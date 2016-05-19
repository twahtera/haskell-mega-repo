{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Main (main) where

import Prelude        ()
import Prelude.Compat

import Data.Maybe              (fromMaybe)
import Data.Semigroup          ((<>))
import Data.Text               (Text)
import Network.Wai             (Application)
import Servant
import Servant.Futurice.Status
import System.Environment      (getArgs, lookupEnv)
import System.Random           (randomRIO)
import Text.Read               (readMaybe)

import qualified Network.Wai.Handler.Warp as Warp

type API = Get '[PlainText] Text :<|> StatusAPI

api :: Proxy API
api = Proxy

stats :: StatusInfoIO
stats = gcStatusInfo <> randomStatusInfo
  where
    randomStatusInfo :: StatusInfoIO
    randomStatusInfo = SIIO $ group "random" . metric "int" <$> r

    r :: IO Int
    r = randomRIO (0, 100)

server :: Server API
server = pure "Example\n" :<|> serveStatus stats

app :: Application
app = serve api server

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("run":_) -> do
            port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
            Warp.run port app
        _ -> putStrLn "To run, pass run argument"
