{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.GitHubDashboard.Orphans () where

import Futurice.Prelude
import Prelude          ()

import Servant.Docs (ToSample (..))

import qualified Database.PostgreSQL.Simple.FromField as Postgres 
import qualified GitHub             as GH
import qualified GitHub.Data.Name as GH

instance AnsiPretty (GH.Name entity)
instance AnsiPretty GH.Language

instance ToSample Text where
    toSamples _ = [("", "foobar")]

instance Postgres.FromField (GH.Name entity) where
    fromField f mbs = GH.N <$> Postgres.fromField f mbs
