{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.PlanMillProxy.Logic (
    haxlEndpoint,
    ) where

import Futurice.Prelude
import Prelude ()

import Control.Monad.PlanMill           (planmillQuery)
import Data.Binary.Tagged               (taggedEncode, HasSemanticVersion, HasStructuralInfo)
import Data.ByteString.Lazy             (ByteString)
import Data.Constraint
import Futurice.App.PlanMillProxy.H
import Futurice.App.PlanMillProxy.Types (Ctx)
import Futurice.Servant                 (cachedIO)
import PlanMill.Types.Query             (Query, SomeQuery (..), queryDict)

haxlEndpoint :: Ctx -> [SomeQuery] -> IO [Either Text ByteString]
haxlEndpoint (cache, cfg) = traverse fetch
  where
    fetch :: SomeQuery -> IO (Either Text ByteString)
    fetch (SomeQuery q) = case (binaryDict, semVerDict, structDict) of 
        (Dict, Dict, Dict) -> taggedEncode <$$> fetch' q
      where
        binaryDict = queryDict (Proxy :: Proxy Binary) (Sub Dict) q
        semVerDict = queryDict (Proxy :: Proxy HasSemanticVersion) (Sub Dict) q
        structDict = queryDict (Proxy :: Proxy HasStructuralInfo) (Sub Dict) q

    fetch' :: Query a -> IO (Either Text a)
    fetch' q = case (nfdataDict, typeableDict) of
        (Dict, Dict) -> do
            res <- tryDeep $ cachedIO cache 3600 q $ runH cfg $ planmillQuery q
            return $ first (\x -> show x ^. packed) res
      where
        nfdataDict = queryDict (Proxy :: Proxy NFData) (Sub Dict) q
        typeableDict = queryDict (Proxy :: Proxy Typeable) (Sub Dict) q
