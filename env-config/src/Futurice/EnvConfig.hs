{-# LANGUAGE GADTs #-}
module Futurice.EnvConfig where

import Futurice.Prelude
import Prelude ()

import Control.Monad.Logger           (LogLevel (..))
import Data.Char                      (toUpper)
import Database.PostgreSQL.Simple     (ConnectInfo (..))
import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)
import System.Environment             (lookupEnv)

import qualified Chat.Flowdock.REST as FD
import qualified Data.ByteString    as B
import qualified FUM
import qualified GitHub             as GH
import qualified PlanMill           as PM

defaultPort :: Int
defaultPort = 8000

-- | Parse port, @parseDefaultPort prefix@ first tries to parse @PREFIX_PORT@,
-- then @PORT@ and after that defaults to 'defaultPort'
parseDefaultPort :: String -> IO Int
parseDefaultPort prefix = do
    val0 <- parseEnvVarMaybe var0
    case val0 of
        Just x -> pure x
        Nothing ->  parseEnvVarWithDefault var1 defaultPort
  where
    var0 = map toUpper prefix ++ "_PORT"
    var1 = "PORT"

-- | Class to parse env variables
class FromEnvVar a where
    fromEnvVar :: String -> Maybe a

-- | Parse required environment variable
parseEnvVar :: FromEnvVar a
            => String  -- ^ Environment variable
            -> IO a
parseEnvVar var =
    parseEnvVarWithDefault var (error $ "No environment variable " ++ var)

-- | Parse optional var.
-- Returns 'Nothing' if var is not set
-- Will throw an exception if var cannot be parsed
parseEnvVarMaybe
    :: FromEnvVar a  -- ^ Environment variable
    => String        -- ^ Default value
    -> IO (Maybe a)
parseEnvVarMaybe var = do
    val <- lookupEnv var
    case val of
        Nothing   -> pure Nothing
        Just val' -> case fromEnvVar val' of
            Nothing -> error $
               "Cannot parse environment variable: " ++ var ++ " -- " ++ val'
            Just x  -> pure (Just x)

-- | Parse optional environment variable.
-- Will fail if variable is present, but is of invalid format.
parseEnvVarWithDefault
    :: FromEnvVar a
     => String  -- ^ Environment variable
    -> a        -- ^ Default value
    -> IO a
parseEnvVarWithDefault var def = fromMaybe def <$> parseEnvVarMaybe var

getConnectInfo :: IO ConnectInfo
getConnectInfo = f
    <$> parseEnvVar "POSTGRES_URL"
    <*> parseEnvVar "POSTGRES_PASS"
  where
    f connInfo pass = connInfo { connectPassword = pass }

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- | This instance is temporary.
instance a ~ Char => FromEnvVar [a] where
    fromEnvVar = Just

instance FromEnvVar Text where
    fromEnvVar = Just . view packed

instance FromEnvVar Int where
    fromEnvVar = readMaybe

instance FromEnvVar B.ByteString where
    fromEnvVar = Just . fromString

instance FromEnvVar Word64 where
    fromEnvVar = readMaybe

instance FromEnvVar ConnectInfo where
    fromEnvVar = parseDatabaseUrl

instance FromEnvVar LogLevel where
    fromEnvVar "DEBUG" = Just LevelDebug
    fromEnvVar "INFO"  = Just LevelInfo
    fromEnvVar "WARN"  = Just LevelWarn
    fromEnvVar "ERROR" = Just LevelError
    fromEnvVar _       = Nothing

instance FromEnvVar Bool where
    fromEnvVar "1" = Just True
    fromEnvVar "0" = Just False
    fromEnvVar _   = Nothing

-------------------------------------------------------------------------------
-- FUM
-------------------------------------------------------------------------------

instance FromEnvVar FUM.AuthToken where
    fromEnvVar = fmap FUM.AuthToken . fromEnvVar

instance FromEnvVar FUM.BaseUrl where
    fromEnvVar = Just . FUM.BaseUrl

instance FromEnvVar FUM.ListName where
    fromEnvVar = fmap FUM.ListName . fromEnvVar

-------------------------------------------------------------------------------
-- PlanMill
-------------------------------------------------------------------------------

instance FromEnvVar PM.ApiKey where
    fromEnvVar = fmap PM.ApiKey . fromEnvVar

instance FromEnvVar (PM.Identifier a) where
    fromEnvVar = fmap PM.Ident . fromEnvVar

-------------------------------------------------------------------------------
-- GitHub
-------------------------------------------------------------------------------

instance FromEnvVar GH.Auth where
    fromEnvVar = fmap GH.OAuth . fromEnvVar

instance FromEnvVar (GH.Name a) where
    fromEnvVar = fmap (GH.mkName Proxy) . fromEnvVar

-------------------------------------------------------------------------------
-- Flowdock
-------------------------------------------------------------------------------

instance FromEnvVar FD.AuthToken where
    fromEnvVar = fmap FD.AuthToken . fromEnvVar

instance FromEnvVar (FD.ParamName a) where
    fromEnvVar = fmap FD.mkParamName . fromEnvVar
