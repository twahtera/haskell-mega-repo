{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module Personio.Types where

import Data.Aeson.Compat
import Data.Time          (zonedTimeToLocalTime)
import Futurice.Aeson
import Futurice.EnvConfig
import Futurice.Generics
import Futurice.IdMap     (HasKey (..))
import Futurice.Prelude
import Prelude ()

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

-- | Personio employee id.
newtype EmployeeId = EmployeeId Word
  deriving (Eq, Ord, Show)

deriveGeneric ''EmployeeId

instance Arbitrary EmployeeId where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance Hashable EmployeeId where
    hashWithSalt salt (EmployeeId i) = hashWithSalt salt i

instance FromJSON EmployeeId where
    parseJSON = fmap EmployeeId . parseJSON

instance ToJSON EmployeeId where
    toJSON (EmployeeId i) = toJSON i
    toEncoding (EmployeeId i) = toEncoding i

instance NFData EmployeeId where
    rnf (EmployeeId i) = rnf i

-- | We could use 'GeneralizedNewtypeDeriving', but we don't (yet?).
instance ToParamSchema EmployeeId where
    toParamSchema = newtypeToParamSchema

instance FromHttpApiData EmployeeId where
    parseUrlPiece = newtypeParseUrlPiece

instance ToHttpApiData EmployeeId where
    toUrlPiece = newtypeToUrlPiece

_EmployeeId :: Prism' Text EmployeeId
_EmployeeId = prism' toUrlPiece (either (const Nothing) Just . parseUrlPiece)

-- | Employee structure. Doesn't contain sensitive information.
data Employee = Employee
    { _employeeId       :: !EmployeeId
    , _employeeFirst    :: !Text
    , _employeeLast     :: !Text
    , _employeeHireDate :: !(Maybe Day)
    , _employeeEndDate  :: !(Maybe Day)
    -- use this when debugging
    -- , employeeRest     :: !(HashMap Text Value)
    }
  deriving (Eq, Show)

makeLenses ''Employee
deriveGeneric ''Employee

instance Arbitrary Employee where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance HasKey Employee where
    type Key Employee = EmployeeId
    key = employeeId

instance FromJSON Employee where
    parseJSON = sopParseJSON

instance ToJSON Employee where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

{-
    parseJSON = withObjectDump "Personio.Employee" $ \obj -> do
        type_ <- obj .: "type"
        if type_ == ("Employee" :: Text)
            then obj .: "attributes" >>= parseObject
            else fail $ "Not Employee: " ++ type_ ^. unpacked
      where
        parseObject obj = Employee
            <$> parseAttribute "id"
            <*> parseAttribute "first_name"
            <*> parseAttribute "last_name"
            <*> fmap (fmap zonedDay) (parseAttribute "hire_date")
            <*> fmap (fmap zonedDay) (parseAttribute "contract_end_date")
                -- <*> pure obj -- for employeeRest field
          where
            parseAttribute :: FromJSON a => Text -> Parser a
            parseAttribute attrName = do
                attr <- obj .: attrName
                withObjectDump "Attribute" (.: "value") attr

            zonedDay =  localDay . zonedTimeToLocalTime
-}

-------------------------------------------------------------------------------
-- Envelope
-------------------------------------------------------------------------------

newtype Envelope a = Envelope { getEnvelope :: a }

instance FromJSON a => FromJSON (Envelope a) where
    parseJSON = withObjectDump "Envelope" $ \obj -> do
        b <- obj .: "success"
        case b of
            False ->  do
                err <- obj .: "error"
                fail (errMessage err ^. unpacked)
            True -> Envelope <$> obj .: "data"

-- | API error.
data Err = Err
    { errCode :: !Int
    , errMessage :: !Text
    }

instance FromJSON Err where
    parseJSON = withObjectDump "Error" $ \obj -> Err
        <$> obj .: "code"
        <*> obj .: "message"

-------------------------------------------------------------------------------
-- Credentials
-------------------------------------------------------------------------------

-- | Access Token
newtype AccessToken = AccessToken { _getAccessToken :: Text }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''AccessToken
instance Hashable AccessToken
instance NFData AccessToken

instance IsString AccessToken where
    fromString = AccessToken . view packed

instance FromJSON AccessToken where
    parseJSON = withObjectDump "Personio.AccessToken" $ \obj ->
        AccessToken <$> obj .: "token"

-- | Client id
newtype ClientId = ClientId { _getClientId :: Text }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''ClientId
instance Hashable ClientId
instance NFData ClientId

instance IsString ClientId where
    fromString = ClientId . view packed

instance FromJSON ClientId where
    parseJSON = withText "Personio.ClientId" $ pure . ClientId

-- | Client id
newtype ClientSecret = ClientSecret { _getClientSecret :: Text }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''ClientSecret
instance Hashable ClientSecret
instance NFData ClientSecret

instance IsString ClientSecret where
    fromString = ClientSecret . view packed

instance FromJSON ClientSecret where
    parseJSON = withText "Personio.ClientSecret" $ pure . ClientSecret

-------------------------------------------------------------------------------
-- Base url
-------------------------------------------------------------------------------

-- | Base url of Persiono service
newtype BaseUrl = BaseUrl { _getBaseUrl :: Text }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''BaseUrl
instance Hashable BaseUrl
instance NFData BaseUrl

instance IsString BaseUrl where
    fromString = BaseUrl . view packed

instance FromJSON BaseUrl where
    parseJSON = withText "FUM BaseUrl" $ pure . BaseUrl

productionBaseUrl :: BaseUrl
productionBaseUrl = BaseUrl "https://api.personio.de"

-------------------------------------------------------------------------------
-- Cfg
-------------------------------------------------------------------------------

data Cfg = Cfg
    { _cfgBaseUrl      :: !BaseUrl
    , _cfgClientId     :: !ClientId
    , _cfgClientSecret :: !ClientSecret
    }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''Cfg
instance Hashable Cfg
instance NFData Cfg

class HasPersonioCfg a where
    personioCfg :: Lens' a Cfg

instance HasPersonioCfg Cfg where
    personioCfg = id

-------------------------------------------------------------------------------
-- env-config instances and helpers
-------------------------------------------------------------------------------

instance FromEnvVar ClientId where
    fromEnvVar = fmap ClientId . fromEnvVar

instance FromEnvVar ClientSecret where
    fromEnvVar = fmap ClientSecret . fromEnvVar

-- |
--
-- @
-- cfg <- withStderrLogger $ \logger -> runLogT "configure" logger $ getConfig' "" configurePersonioCfg
-- @
configurePersonioCfg :: ConfigParser Cfg
configurePersonioCfg = Cfg productionBaseUrl
    <$> envVar "PERSONIO_CLIENT_ID"
    <*> envVar "PERSONIO_CLIENT_SECRET"

instance Configure Cfg where
    configure = configurePersonioCfg
