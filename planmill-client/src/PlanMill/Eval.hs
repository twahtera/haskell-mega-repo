{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module PlanMill.Eval (evalPlanMill) where

import PlanMill.Internal.Prelude

import Control.Monad.Http  (MonadHttp (..), httpLbs)
import Data.Aeson.Compat   (eitherDecode)
import Network.HTTP.Client
       (Request, RequestBody (..), checkStatus, method, parseUrl, path,
       queryString, requestBody, requestHeaders, responseBody, responseStatus,
       setQueryString)
import Network.HTTP.Types  (Header, statusIsSuccessful)

-- Qualified imports
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Vector                as V

-- PlanMill import
import PlanMill.Auth    (Auth (..), getAuth)
import PlanMill.Classes
import PlanMill.Types

evalPlanMill
    :: forall m env a.
        ( MonadHttp m, MonadThrow m, MonadTime m, MonadLogger m
        , MonadReader env m, HasPlanMillBaseUrl env, HasCredentials env
        , MonadCRandom' m
        , Applicative m
        , FromJSON a
        )
    => PlanMill a -> m a
evalPlanMill pm = do
    baseReq <- mkBaseReq pm
    let url = BS8.unpack (path baseReq <> queryString baseReq)
    let emptyError = DecodeError url "empty input"
    case pm of
        PlanMillGet qs _ ->
            singleReq baseReq qs (throwM emptyError)
        PlanMillPagedGet qs _ ->
            pagedReq baseReq qs
        PlanMillPost body _ ->
            let req = addHeader ("Content-Type", "application/json;charset=UTF-8") $
                    baseReq { method = "POST", requestBody = RequestBodyLBS body }
            in singleReq req [] (throwM emptyError)
  where
    mkBaseReq :: forall b. PlanMill b -> m Request
    mkBaseReq planmill = do
        cfg <- askCfg
        let baseUrl = getPlanMillBaseUrl cfg
        parseUrl $ baseUrl <> fromUrlParts (requestUrlParts planmill)

    singleReq :: forall b. FromJSON b
              => Request
              -> QueryString
              -> m b           -- ^ Action to return in case of empty response
              -> m b
    singleReq req qs d = do
        -- We need to generate auth (nonce) at each req
        auth <- getAuth
        let req' = setQueryString' qs (req `withAuthHeader` auth)
        let url = BS8.unpack (path req') <> BS8.unpack (queryString req')
        $(logInfo) $ T.pack $ "Request: " <> url
        res <- httpLbs req'
        $(logDebug) $ "response status: " <> textShow (responseStatus res)
        if isn't _Empty (responseBody res)
            then do
                $(logDebug) $ "response body: " <> TE.decodeUtf8 (responseBody res ^. strict)
                if statusIsSuccessful (responseStatus res)
                    then parseResult url $ responseBody res
                    else throwM $ parseError url $ responseBody res
            else do
                $(logDebug) "empty response"
                d

    setQueryString' :: QueryString -> Request -> Request
    setQueryString' qs = setQueryString (f <$> qs)
      where
        f (a, b) = (TE.encodeUtf8 a, Just $ TE.encodeUtf8 b)

    parseResult :: forall b .(FromJSON b) => String -> LBS.ByteString -> m b
    parseResult url body =
        case eitherDecode body of
            Right x  -> pure x
            Left err -> throwM $ DecodeError url err

    parseError :: String -> LBS.ByteString -> PlanMillError
    parseError url body =
        case eitherDecode body of
            Right exc -> ErrorResponse url exc
            Left err  -> DecodeError url err

    -- See https://online.planmill.com/pmtrial/schemas/v1_5/index.html#projects_get
    -- for explanation of paged response query string parameters
    --
    -- We actually need the type equality constraint
    -- to use vector's length
    pagedReq :: forall b b'.
                (b ~ V.Vector b', FromJSON b)
             => Request -> QueryString
             -> m b
    pagedReq req qs = go mempty
      where
        go :: V.Vector b' -> m b
        go acc = do
            -- We are for one too much, because if `nextrows` is over amount
            -- the collection from beginning is returned
            let qs' = [ ("rowcount", T.pack $ show $ rowCount + 1)
                      , ("nextrows", T.pack $ show $ V.length acc + 1)
                      ]
            res <- singleReq req (qs ++ qs') (pure V.empty)
            if V.length res <= rowCount
                then pure (acc <> res)
                else go (acc <> V.take rowCount res)

        rowCount :: Int
        rowCount = 100

authHeader :: Auth -> Header
authHeader (Auth (Ident uid) (Nonce nonce) ts sig) = ("x-PlanMill-Auth",
    "user:"      <> bsShow uid                   <> ";" <>
    "nonce:"     <> nonce                        <> ";" <>
    "timestamp:" <> bsShow (utcTimeToInteger ts) <> ";" <>
    "signature:" <> Base64.encode sig)

withAuthHeader :: Request -> Auth -> Request
withAuthHeader req auth =
    req { requestHeaders = authHeader auth : requestHeaders req
        , checkStatus = \_ _ _ -> Nothing
        }

addHeader :: Header -> Request -> Request
addHeader header req = req
    { requestHeaders = header : requestHeaders req }
