{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.Futucli.Orphans () where

import Futurice.Prelude
import Prelude          ()

import Data.Aeson  (FromJSON (..), withText)
import Data.Binary (Binary (..))

import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding   as TE
import qualified GitHub               as GH

instance FromJSON GH.Auth  where
    parseJSON = withText "GitHub token" $ pure . GH.OAuth . TE.encodeUtf8

instance Binary (GH.Request k a) where
    get = undefined

    put (GH.Query ps qs) =
        put (0 :: Int) >> put ps >> put qs
    put (GH.PagedQuery ps qs c) =
        put (1 :: Int) >> put ps >> put qs >> put c
    put (GH.Command m ps bs) =
        put (2 :: Int) >> put m >> put ps >> put bs
    put (GH.StatusQuery sm r) =
        put (3 :: Int) >> put sm >> put r
    put (GH.HeaderQuery hs r) =
        put (4 :: Int) >> put hs >> put r

instance Binary (GH.CommandMethod a) where
    get = undefined
    put GH.Post   = put (0 :: Int)
    put GH.Patch  = put (1 :: Int)
    put GH.Put    = put (2 :: Int)
    put GH.Delete = put (3 :: Int)

instance Binary (GH.StatusMap a) where
    get = undefined
    put GH.StatusOnlyOk = put (0 :: Int)
    put GH.StatusMerge  = put (1 :: Int)

instance (CI.FoldCase a, Binary a) => Binary (CI.CI a) where
    get = CI.mk <$> get
    put = put . CI.original
