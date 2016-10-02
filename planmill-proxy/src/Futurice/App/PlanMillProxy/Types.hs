{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.App.PlanMillProxy.Types (
    Ctx (..),
    SomeBinaryTagged (..),
    someBinaryTagged,
    ) where

import Futurice.Prelude
import Prelude ()

import Control.Monad.Logger       (LogLevel (..))
import Data.Binary.Tagged
       (HasSemanticVersion (..), HasStructuralInfo (..), taggedEncode)
import Data.ByteString.Lazy       (ByteString)
import Data.Pool                  (Pool)
import Data.Swagger               (NamedSchema (..), ToSchema (..))
import Data.Swagger.Schema        (binarySchema)
import Database.PostgreSQL.Simple (Connection)
import Futurice.Servant           (DynMapCache)
import PlanMill                   (Cfg)

-- | /TODO/ move to servant-binary-tagged
newtype SomeBinaryTagged = SomeBinaryTagged ByteString
  deriving (Eq, Show, Typeable, Generic)

instance NFData SomeBinaryTagged
instance Binary SomeBinaryTagged

instance HasSemanticVersion SomeBinaryTagged where
    type SemanticVersion SomeBinaryTagged = SemanticVersion ByteString
instance HasStructuralInfo SomeBinaryTagged where
    structuralInfo _ = structuralInfo (Proxy :: Proxy ByteString)

instance ToSchema SomeBinaryTagged where
    declareNamedSchema _ = pure $ NamedSchema (Just "BinaryTagged") binarySchema

someBinaryTagged
    :: (Binary a, HasSemanticVersion a, HasStructuralInfo a)
    => a -> SomeBinaryTagged
someBinaryTagged = SomeBinaryTagged . taggedEncode

data Ctx = Ctx
    { ctxCache        :: !DynMapCache
    , ctxPlanmillCfg  :: !Cfg
    , ctxPostgresPool :: !(Pool Connection)  -- TODO: write a lib to handle these
    , ctxLogLevel     :: !LogLevel
    }
