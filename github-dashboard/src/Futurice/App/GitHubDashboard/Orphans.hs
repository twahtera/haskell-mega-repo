{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.GitHubDashboard.Orphans () where

import Futurice.Prelude
import Prelude          ()

import Servant.Docs (ToSample (..))

instance ToSample Text where
    toSamples _ = [("", "foobar")]


