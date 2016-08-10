{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Futurice.Prelude
import Prelude ()

import Lucid              hiding (for_)
import Network.Wai        (Application)
import Servant
import Servant.Cache
import Servant.HTML.Lucid
import System.Environment (getArgs, lookupEnv)

import qualified Data.Text                as T
import qualified Network.Wai.Handler.Warp as Warp

type UpperAPI = "upper" :> Capture "word" Text :> Get '[PlainText] Text

type API = Cached SomeCache 3600 :> (Get '[HTML] (Html ()) :<|> UpperAPI)

api :: Proxy API
api = Proxy

upperEndpoint :: Proxy UpperAPI
upperEndpoint = Proxy

server :: Server API
server = pure indexPage :<|> pure . T.toUpper

app :: SomeCache -> Application
app cache = serveWithContext api context server
  where
    context :: Context '[SomeCache]
    context = cache :. EmptyContext

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("run":_) -> do
            port <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
            cache <- mkCache
            Warp.run port (app cache)
        _ -> putStrLn "To run, pass run argument"

indexPage :: Html ()
indexPage = doctypehtml_ $ body_ $ ul_ $ for_ exampleWords $ \word ->
    let link = T.pack $ show $ safeLink api upperEndpoint word
    in li_ $ a_ [href_ link] $ toHtml word
  where
    exampleWords = ["kissa", "kassi", "kassa"]
