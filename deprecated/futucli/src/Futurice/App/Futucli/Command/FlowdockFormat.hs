{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.App.Futucli.Command.FlowdockFormat (flowdockFormat) where

import Futurice.Prelude

import Futurice.App.Futucli.Cfg

import Control.Lens            (to)
import Data.Aeson.Extra.Stream (streamDecode)

import qualified Chat.Flowdock.REST          as FD
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Text.IO                as T

flowdockFormat :: FilePath -> Cfg ->  IO ()
flowdockFormat fp _ = do
    contents <- LBS.readFile fp
    let ~(events, err) = streamDecode contents
    traverse_ p events
    maybe (pure ()) print err

p :: FD.Message -> IO ()
p msg@FD.Message { FD._msgContent = FD.MTMessage contents } =
    T.putStrLn $ "<" <> msg ^. FD.msgUser . to FD.getIdentifier .  to textShow <>  "> " <> contents
p _ = pure ()
