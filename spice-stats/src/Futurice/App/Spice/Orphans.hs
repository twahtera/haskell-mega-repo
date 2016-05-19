{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.Spice.Orphans () where

import Futurice.Prelude
import Prelude          ()

import Data.Aeson   (ToJSON (..), object, (.=))
import Servant.Docs (ToSample (..))

import qualified Chat.Flowdock.REST as FD
import qualified GitHub             as GH

instance AnsiPretty (GH.Name entity)
instance AnsiPretty GH.Language

instance ToJSON FD.Author where
    toJSON (FD.Author n e a) = object
        [ "name" .= n
        , "email" .= e
        , "avatar" .= a
        ]

instance ToSample Text where
    toSamples _ = [("", "foobar")]
