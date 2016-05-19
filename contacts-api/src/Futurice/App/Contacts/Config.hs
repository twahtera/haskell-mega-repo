{-# LANGUAGE GADTs #-}
module Futurice.App.Contacts.Config (
    Config(..),
    getConfig,
    defaultPort,
    ) where

import Futurice.Prelude
import Prelude          ()

import System.Environment (lookupEnv)

import qualified Chat.Flowdock.REST as FD
import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified FUM
import qualified GitHub             as GH

-- | TODO: split config into two parts
data Config = Config
    { cfgFumAuth     :: !FUM.AuthToken     -- ^ FUM auth token
    , cfgFumBaseUrl  :: !FUM.BaseUrl       -- ^ FUM base url
    , cfgFumUserList :: !FUM.ListName      -- ^ FUM user list
    , cfgGhAuth      :: !GH.Auth           -- ^ Github auth information
    , cfgGhOrg       :: !(GH.Name GH.Organization)
      -- ^ Github organisation
    , cfgFdAuth      :: !FD.AuthToken      -- ^ Flowdock token
    , cfgFdOrg       :: !(FD.ParamName FD.Organisation)
      -- ^ Flowdock organisation
    , cfgPort        :: !Int
      -- ^ Port to listen from, default is 'defaultPort'.
    }
    deriving (Show)

getConfig :: IO Config
getConfig =
    Config <$> parseEnvVar "FUM_AUTH_TOKEN"
           <*> parseEnvVar "FUM_BASE_URL"
           <*> parseEnvVar "FUM_USER_LIST"
           <*> parseEnvVar "GH_AUTH_TOKEN"
           <*> parseEnvVar "GH_ORGANISATION"
           <*> parseEnvVar "FD_AUTH_TOKEN"
           <*> parseEnvVar "FD_ORGANISATION"
           <*> parseEnvVarWithDefault "PORT" defaultPort

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

instance FromEnvVar FUM.AuthToken where
    fromEnvVar = fmap FUM.AuthToken . fromEnvVar

instance FromEnvVar FUM.BaseUrl where
    fromEnvVar = Just . FUM.BaseUrl

instance FromEnvVar FUM.ListName where
    fromEnvVar = fmap FUM.ListName . fromEnvVar

instance FromEnvVar GH.Auth where
    fromEnvVar = fmap GH.OAuth . fromEnvVar

instance FromEnvVar (GH.Name a) where
    fromEnvVar = fmap (GH.mkName Proxy) . fromEnvVar

instance FromEnvVar FD.AuthToken where
    fromEnvVar = fmap FD.AuthToken . fromEnvVar

instance FromEnvVar (FD.ParamName a) where
    fromEnvVar = fmap FD.mkParamName . fromEnvVar

instance FromEnvVar Int where
    fromEnvVar = readMaybe
