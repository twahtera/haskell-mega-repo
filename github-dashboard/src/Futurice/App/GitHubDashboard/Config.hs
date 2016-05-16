{-# LANGUAGE GADTs #-}
module Futurice.App.GitHubDashboard.Config (
    Config(..),
    getConfig,
    defaultPort,
    ) where

import Futurice.Prelude
import Prelude          ()

import Database.PostgreSQL.Simple     (ConnectInfo (..))
import Database.PostgreSQL.Simple.URL (parseDatabaseUrl)
import System.Environment (lookupEnv)

import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified GitHub             as GH

data Config = Config
    { cfgGhAuth           :: !GH.Auth      -- ^ Github auth information
    , cfgPostgresConnInfo :: !ConnectInfo
    , cfgPort             :: !Int
      -- ^ Port to listen from, default is 'defaultPort'.
    }
    deriving (Show)

getConfig :: IO Config
getConfig = Config
    <$> parseEnvVar "GH_AUTH_TOKEN"
    <*> getConnectInfo
    <*> parseEnvVarWithDefault "PORT" defaultPort

getConnectInfo :: IO ConnectInfo
getConnectInfo = f
    <$> parseEnvVar "POSTGRES_URL"
    <*> parseEnvVar "POSTGRES_PASS"
  where
    f connInfo pass = connInfo { connectPassword = pass }

defaultPort :: Int
defaultPort = 8000

-- | Class to parse env variables
class FromEnvVar a where
    fromEnvVar :: String -> Maybe a

-- | Parse required environment variable
parseEnvVar :: FromEnvVar a
            => String  -- ^ Environment variable
            -> IO a
parseEnvVar var =
    parseEnvVarWithDefault var (error $ "No environment variable " ++ var)

-- | Parse optional environment variable.
-- Will fail if variable is present, but is of invalid format.
parseEnvVarWithDefault :: FromEnvVar a
                       => String  -- ^ Environment variable
                       -> a       -- ^ Default value
                       -> IO a
parseEnvVarWithDefault var def = do
    val <- lookupEnv var
    case val of
        Nothing   -> pure def
        Just val' -> case fromEnvVar val' of
            Nothing -> error $
               "Cannot parse environment variable: " ++ var ++ " -- " ++ val'
            Just x  -> pure x

-- | This instance is temporary.
instance a ~ Char => FromEnvVar [a] where
    fromEnvVar = Just

instance FromEnvVar T.Text where
    fromEnvVar = Just . T.pack

instance FromEnvVar B.ByteString where
    fromEnvVar = Just . fromString

instance FromEnvVar GH.Auth where
    fromEnvVar = fmap GH.OAuth . fromEnvVar

instance FromEnvVar (GH.Name a) where
    fromEnvVar = fmap (GH.mkName Proxy) . fromEnvVar

instance FromEnvVar Int where
    fromEnvVar = readMaybe

instance FromEnvVar ConnectInfo where
    fromEnvVar = parseDatabaseUrl
