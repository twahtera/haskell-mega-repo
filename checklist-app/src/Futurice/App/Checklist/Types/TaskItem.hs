{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Checklist.Types.TaskItem where

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
data TaskItem
    = TaskItemDone
    | TaskItemTodo
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

makePrisms ''TaskItem
deriveGeneric ''TaskItem

instance Arbitrary TaskItem where
    arbitrary = sopArbitrary
    shrink    = sopShrink

taskItemToText :: TaskItem -> Text
taskItemToText TaskItemDone = "done"
taskItemToText TaskItemTodo = "todo"

taskItemFromText :: Text -> Maybe TaskItem
taskItemFromText t = Map.lookup (T.toLower t) m
  where
    m = Map.fromList $ map (\x -> (T.toLower $ taskItemToText x, x)) [minBound .. maxBound]

_TaskItem :: Prism' Text TaskItem
_TaskItem = prism' taskItemToText taskItemFromText

instance ToParamSchema TaskItem where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & enum_ ?~ (String . taskItemToText <$> [minBound .. maxBound])

instance ToJSON TaskItem where
    toJSON = String . taskItemToText

instance FromJSON TaskItem where
    parseJSON = withText "TaskItem" $ \t ->
        maybe (fail $ "invalid taskItem " <> t ^. unpacked) pure $ t ^? _TaskItem

instance FromHttpApiData TaskItem where
    parseUrlPiece t =
        maybe (Left $ "invalid taskItem " <> t) Right $ t ^? _TaskItem

instance ToHttpApiData TaskItem where
    toUrlPiece = taskItemToText
