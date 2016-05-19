{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module FUM.Request (
    FUM(..),
    URL,
    evalFUM,
    ) where

import Futurice.Prelude
import Prelude          ()

import Control.Monad.Http.Class (MonadHttp, httpLbs)
import Control.Monad.Reader     (MonadReader (..))
import Data.Aeson.Compat        (FromJSON (..), decode, withObject, (.:))

import qualified Data.Text.Encoding  as TE (encodeUtf8)
import qualified Data.Vector         as V (empty)
import qualified Network.HTTP.Client as H (Request (..), parseUrl, responseBody)

import FUM.Types

data FUM a where
    FumGet      :: URL -> FUM a
    FumPagedGet :: URL -> FUM (Vector a)

deriving instance Eq (FUM a)
deriving instance Ord (FUM a)
deriving instance Show (FUM a)

instance Hashable (FUM a) where
    hashWithSalt salt (FumGet url) =
        salt `hashWithSalt` (0 :: Int) `hashWithSalt` url
    hashWithSalt salt (FumPagedGet url) =
        salt `hashWithSalt` (1 :: Int) `hashWithSalt` url

type URL = String

evalFUM
    :: forall m env a.
       ( MonadHttp m, MonadThrow m
       , MonadReader env m, HasAuthToken env, HasBaseUrl env
       , FromJSON a
       )
    => FUM a
    -> m a
evalFUM (FumGet url)      = getSingle url
evalFUM (FumPagedGet url) = getMulti url

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

mkReq
    :: (MonadThrow m, MonadReader env m, HasAuthToken env, HasBaseUrl env)
    => URL
    -> m H.Request
mkReq url = do
    AuthToken token <- view authToken
    BaseUrl base <- view baseUrl
    baseReq <- H.parseUrl $ base <> url
    let authHeader = ("Authorization", TE.encodeUtf8 $ "Token " <> token)
    return $ baseReq { H.requestHeaders = authHeader : H.requestHeaders baseReq }

getMulti
    :: ( MonadHttp m, MonadThrow m
       , MonadReader env m, HasAuthToken env, HasBaseUrl env
       , FromJSON (Vector a)
       )
    => URL 
    -> m (Vector a)
getMulti url = do
    req <- mkReq url
    res <- httpLbs req
    mr <- decode (H.responseBody res)
    (mrValue mr <>) <$> maybe (return V.empty) getMulti (mrNext mr)

getSingle
    :: ( MonadHttp m, MonadThrow m
       , MonadReader env m, HasAuthToken env, HasBaseUrl env
       , FromJSON a
       )
    => URL 
    -> m a
getSingle url = do
    req <- mkReq url
    res <- httpLbs req
    let resBody = H.responseBody res
    decode resBody

-------------------------------------------------------------------------------
-- MultiResult
-------------------------------------------------------------------------------

data MultiResult a = MultiResult
    { mrValue :: !(Vector a)
    , mrNext  :: !(Maybe URL)
      -- ^ Not strict as used as control structure only
    }
    deriving (Eq, Show)

instance FromJSON (Vector a) => FromJSON (MultiResult a) where
    parseJSON = withObject "FUM users result object" $ \v -> MultiResult
        <$> v .: "results"
        <*> v .: "next"
