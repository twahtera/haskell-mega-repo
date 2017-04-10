-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Various classes, and re-exports of often used ones.
module PlanMill.Classes (
    HasPlanMillCfg (..),
    MonadCRandom(..),
    ContainsCryptoGenError,
    CRandT,
    evalCRandT,
    MonadTime(..),
    ) where

import PlanMill.Internal.Prelude
import Futurice.CryptoRandom
       (CRandT, ContainsCryptoGenError, MonadCRandom (..), evalCRandT)
import PlanMill.Types

class HasPlanMillCfg a where
    planmillCfg :: Lens' a Cfg

    planmillCfgUserId :: Lens' a UserId
    planmillCfgUserId = planmillCfg
        . lens cfgUserId (\c x -> c { cfgUserId = x })

    planmillCfgApiKey :: Lens' a ApiKey
    planmillCfgApiKey = planmillCfg
        . lens cfgApiKey (\c x -> c { cfgApiKey = x })

    planmillCfgBaseUrl :: Lens' a String
    planmillCfgBaseUrl = planmillCfg
        . lens cfgBaseUrl (\c x -> c { cfgBaseUrl = x })

instance HasPlanMillCfg Cfg where
    planmillCfg = id
