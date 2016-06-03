{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.App.Futucli.Command.FlowdockFormat (flowdockFormat) where

import Futurice.Prelude

import Futurice.App.Futucli.Cfg

import Control.Lens      (re, to, _Show)
import Data.Aeson.Compat (FromJSON, Result (..), Value, fromJSON)
import Data.Aeson.Parser (value)

import qualified Chat.Flowdock.REST          as FD
import qualified Chat.Flowdock.REST.Internal as FD
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Text.IO                as T

import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.Attoparsec.ByteString.Lazy  as A

flowdockFormat :: FilePath -> Cfg ->  IO ()
flowdockFormat fp _ = do
    contents <- LBS.readFile fp
    let ~(events, err) = streamDecode contents
    traverse_ p events
    maybe (pure ()) print err

p :: FD.Message -> IO ()
p msg@FD.Message { FD._msgContent = FD.MTMessage contents } =
    T.putStrLn $ "<" <> msg ^. FD.msgUser . to FD.getIdentifier .  re _Show . packed <>  "> " <> contents
p _ = pure ()

-------------------------------------------------------------------------------
-- Streaming parsing of JSON array:
-------------------------------------------------------------------------------

streamParse :: LBS.ByteString -> ([Value], Maybe String)
streamParse = start
  where
    start bs = case A.parse (lexemeChar '[') bs of
        A.Done bs' _    -> go bs'
        A.Fail _ _ err  -> ([], Just err)
    go bs = case A.parse valueEnd bs of
        A.Done _   (r, False) -> ([r], Nothing)
        A.Done bs' (r, True)  -> case go bs' of
            ~(rs, end)   -> (r:rs, end)
        A.Fail _ _ err  -> ([], Just err)
    valueEnd = do
        v <- value
        c <- True <$ lexemeChar ',' <|> False <$ lexemeChar ']'
        return (v, c)
    lexemeChar c = many A8.space *> A8.char c *> many A8.space


streamDecode :: forall a. FromJSON a => LBS.ByteString -> ([a], Maybe String)
streamDecode bs = go values
  where
    (values, err)  = streamParse bs
    go :: [Value] -> ([a], Maybe String)
    go []     = ([], err)
    go (v:vs) = case fromJSON v of
        Error err'  -> ([], Just err')
        Success x  -> case go vs of
            ~(xs, err') -> (x:xs, err')
