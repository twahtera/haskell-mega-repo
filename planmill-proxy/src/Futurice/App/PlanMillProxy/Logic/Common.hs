{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=0 #-}
#endif
module Futurice.App.PlanMillProxy.Logic.Common where

import Prelude ()
import Futurice.Prelude
import Control.Monad.Catch    (handle)
import Control.Monad.PlanMill (planmillQuery)
import Data.Binary.Get        (Get, runGetOrFail)
import Data.Binary.Tagged
       (HasSemanticVersion, HasStructuralInfo, SemanticVersion, Version,
       structuralInfo, structuralInfoSha1ByteStringDigest)
import Data.Constraint
import Futurice.Servant       (CachePolicy (..), genCachedIO)
import GHC.TypeLits           (natVal)

import PlanMill.Types.Query
       (Query (..), queryDict)

import qualified Database.PostgreSQL.Simple as Postgres

import Futurice.App.PlanMillProxy.H
import Futurice.App.PlanMillProxy.Types (Ctx (..))

-------------------------------------------------------------------------------
-- Intervals
-------------------------------------------------------------------------------

genericAge :: String
genericAge = "'6 hours'"

capacityAge :: String
capacityAge = "'12 hours'"

-- Timereports are updated by updating oldest ones, so no age guarantee
-- atm

-------------------------------------------------------------------------------
-- LIO
-------------------------------------------------------------------------------

type LIO = LogT IO

runLIO :: Ctx -> LIO a -> IO a
runLIO ctx =  runLogT' ctx 

-------------------------------------------------------------------------------
-- Utiltities
-------------------------------------------------------------------------------

-- | Run query on real planmill backend.
fetchFromPlanMill :: Ctx -> Query a -> LIO a
fetchFromPlanMill ctx q = case typeableDict of
    Dict -> liftIO
        -- TODO: add cache cleanup
        $ genCachedIO RequestNew logger cache (10 * 60) q
        $ runH lgr cfg $ planmillQuery q
  where
    typeableDict = queryDict (Proxy :: Proxy Typeable) q
    logger       = ctxLogger ctx
    cache        = ctxCache ctx
    cfg          = ctxPlanmillCfg ctx
    lgr          = ctxLogger ctx

handleSqlError :: a -> IO a -> LIO a
handleSqlError x action = handle (omitSqlError x) $ liftIO action

omitSqlError :: a -> Postgres.SqlError -> LIO a
omitSqlError a err = do
    logAttention_ $ textShow err
    return a

runLogT' :: Ctx -> LogT IO a -> IO a
runLogT' ctx = runLogT "logic" (ctxLogger ctx)

-------------------------------------------------------------------------------
-- binary-tagged additions
-------------------------------------------------------------------------------

-- | Check whether the tag at the beginning of the 'LazyByteString' is correct.
checkTagged
    :: forall a. (HasStructuralInfo a, HasSemanticVersion a)
    => Proxy a -> LazyByteString -> Bool
checkTagged _ lbs = either (const False) (view _3) $ runGetOrFail decoder lbs
  where
    decoder :: Get Bool
    decoder = do
        ver <- get
        hash' <- get
        pure $ ver == ver' && hash' == hash''

    proxyV = Proxy :: Proxy (SemanticVersion a)
    proxyA = Proxy :: Proxy a
    ver' = fromIntegral (natVal proxyV) :: Version
    hash'' = structuralInfoSha1ByteStringDigest . structuralInfo $ proxyA
