{-# LANGUAGE OverloadedStrings  #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Cfg (Cfg(..)) where

import PlanMill.Internal.Prelude

import PlanMill.Types.Auth (ApiKey)
import PlanMill.Types.User (UserId)

-- | Simplest possible configuration type
data Cfg = Cfg
    { cfgUserId  :: UserId
    , cfgApiKey  :: ApiKey
    , cfgBaseUrl :: String
    }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)
