-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Various classes, and re-exports of often used ones.
module PlanMill.Classes (
    HasCredentials(..),
    HasPlanMillBaseUrl(..),
    MonadCRandom(..),
    MonadCRandom',
    CRandT,
    evalCRandT,
    MonadTime(..),
    askCfg,
    ) where

import PlanMill.Internal.Prelude
import Futurice.CryptoRandom
       (CRandT, MonadCRandom (..), MonadCRandom', evalCRandT)
import PlanMill.Types

-- | User operating with API
class HasCredentials env where
    getUserId :: env -> UserId
    getApiKey :: env -> ApiKey

instance HasCredentials Cfg where
    getUserId = cfgUserId
    getApiKey = cfgApiKey

-- | API endpoint.
class HasPlanMillBaseUrl env where
    getPlanMillBaseUrl :: env -> String

instance HasPlanMillBaseUrl Cfg where
    getPlanMillBaseUrl = cfgBaseUrl

-- | Ask for'Cfg'
askCfg
    :: (MonadReader env m, HasPlanMillBaseUrl env, HasCredentials env)
    => m Cfg
askCfg = do
    env <- ask
    return Cfg
        { cfgUserId  = getUserId env
        , cfgApiKey  = getApiKey env
        , cfgBaseUrl = getPlanMillBaseUrl env
        }
