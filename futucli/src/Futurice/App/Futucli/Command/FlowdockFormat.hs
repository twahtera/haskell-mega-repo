{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Futucli.Command.FlowdockFormat (flowdockFormat) where

import Futurice.Prelude

import Futurice.App.Futucli.Cfg

import Control.Lens      (re, to, _Show)
import Data.Aeson.Compat (Value, decode)

import qualified Chat.Flowdock.REST          as FD
import qualified Chat.Flowdock.REST.Internal as FD
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Text.IO                as T

d :: LBS.ByteString -> IO (Vector FD.Message)
d = decode

flowdockFormat :: FilePath -> Cfg ->  IO ()
flowdockFormat fp _ = do
    contents <- LBS.readFile fp
    events <- d contents
    traverse_ p events

p :: FD.Message -> IO ()
p msg@FD.Message { FD._msgContent = FD.MTMessage value } =
    T.putStrLn $ "<" <> msg ^. FD.msgUser . to FD.getIdentifier .  re _Show . packed <>  "> " <> value
p _ = pure ()
