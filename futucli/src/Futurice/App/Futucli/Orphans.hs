{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.Futucli.Orphans () where

import Prelude ()
import Futurice.Prelude

import Data.Aeson  (FromJSON (..), withText)

import qualified Data.Text.Encoding   as TE
import qualified GitHub               as GH

-- | TODO: Remove after futucli uses tajna
instance FromJSON GH.Auth  where
    parseJSON = withText "GitHub token" $ pure . GH.OAuth . TE.encodeUtf8
