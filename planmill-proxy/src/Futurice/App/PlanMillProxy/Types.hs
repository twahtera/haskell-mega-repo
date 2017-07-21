{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.App.PlanMillProxy.Types (
    Ctx (..),
    Stats (..),
    ) where

import Data.Pool                  (Pool)
import Database.PostgreSQL.Simple (Connection)
import Futurice.Generics
import Futurice.PostgresPool
import Futurice.Prelude
import Futurice.Servant           (DynMapCache)
import PlanMill                   (Cfg)
import PlanMill.Worker            (Workers)
import Prelude ()

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxCache        :: !DynMapCache
    , ctxPlanmillCfg  :: !Cfg
    , ctxPostgresPool :: !(Pool Connection)  -- TODO: write a lib to handle these
    , ctxLogger       :: !Logger
    , ctxWorkers      :: !Workers
    }

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgresPool

-------------------------------------------------------------------------------
-- Stats
-------------------------------------------------------------------------------

data Stats = Stats
    { statsCachedAvgAge  :: Double
    , statsCachedMinAge  :: Double
    , statsCachedMaxAge  :: Double
    , statsCachedTotal   :: Int
    , statsTimereportsAvgAge  :: Double
    , statsTimereportsMinAge  :: Double
    , statsTimereportsMaxAge  :: Double
    , statsTimereportsTotal   :: Int
    }
  deriving Show

deriveGeneric ''Stats

instance ToJSON Stats where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance ToSchema Stats where
    declareNamedSchema = sopDeclareNamedSchema
