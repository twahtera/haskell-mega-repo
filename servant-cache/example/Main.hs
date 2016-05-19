{-# LANGUAGE CPP                #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Prelude        ()
import Prelude.Compat

import Control.Monad         (forM_)
import Data.Functor.Identity (Identity)
import Data.Maybe            (fromMaybe)
import Data.Text             (Text)
import Data.Typeable         (Typeable)
import Lucid
import Network.Wai           (Application)
import Servant
import Servant.Cache
import Servant.HTML.Lucid
import System.Environment    (getArgs, lookupEnv)
import Text.Read             (readMaybe)

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
indexPage = doctypehtml_ $ body_ $ ul_ $ forM_ exampleWords $ \word ->
    let link = T.pack $ show $ safeLink api upperEndpoint word
    in li_ $ a_ [href_ link] $ toHtml word
  where
    exampleWords = ["kissa", "kassi", "kassa"]

-- Orphans
#if !MIN_VERSION_transformers_compat(0,5,0)
deriving instance Typeable Identity
#endif
deriving instance Typeable HtmlT
