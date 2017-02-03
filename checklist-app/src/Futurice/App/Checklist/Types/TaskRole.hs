{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Checklist.Types.TaskRole where

import Prelude ()
import Futurice.Prelude
import Data.Aeson.Compat (Value (String), withText)
import Data.Swagger
       (SwaggerType (SwaggerString), ToParamSchema (..), enum_, type_)
import Futurice.Generics
import Servant           (FromHttpApiData (..), ToHttpApiData (..))

import qualified Data.Map  as Map
import qualified Data.Text as T

-- | States of the tasks
data TaskRole
    = TaskRoleIT
    | TaskRoleHR
    | TaskRoleSupervisor
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

makePrisms ''TaskRole
deriveGeneric ''TaskRole

instance Arbitrary TaskRole where
    arbitrary = sopArbitrary
    shrink    = sopShrink

taskRoleToText :: TaskRole -> Text
taskRoleToText TaskRoleIT         = "IT"
taskRoleToText TaskRoleHR         = "HR"
taskRoleToText TaskRoleSupervisor = "Supervisor"

taskRoleFromText :: Text -> Maybe TaskRole
taskRoleFromText t = Map.lookup (T.toLower t) m
  where
    m = Map.fromList $ map (\x -> (T.toLower $ taskRoleToText x, x)) [minBound .. maxBound]

_TaskRole :: Prism' Text TaskRole
_TaskRole = prism' taskRoleToText taskRoleFromText

instance ToParamSchema TaskRole where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & enum_ ?~ (String . taskRoleToText <$> [minBound .. maxBound])

instance ToJSON TaskRole where
    toJSON = String . taskRoleToText

instance FromJSON TaskRole where
    parseJSON = withText "TaskRole" $ \t ->
        maybe (fail $ "invalid taskRole " <> t ^. unpacked) pure $ t ^? _TaskRole

instance FromHttpApiData TaskRole where
    parseUrlPiece t =
        maybe (Left $ "invalid taskRole " <> t) Right $ t ^? _TaskRole

instance ToHttpApiData TaskRole where
    toUrlPiece = taskRoleToText
