{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Checklist.Types.TaskComment where

import Prelude ()
import Futurice.Prelude
import Futurice.Generics
import Futurice.Lucid.Foundation (ToHtml (..))

newtype TaskComment = TaskComment Text deriving (Eq, Show)

deriveGeneric ''TaskComment

instance ToJSON TaskComment where
    toJSON (TaskComment c) = toJSON c
instance FromJSON TaskComment where
    parseJSON = fmap TaskComment . parseJSON
instance Arbitrary TaskComment where
    arbitrary = sopArbitrary

instance ToHtml TaskComment where
    toHtml (TaskComment c) = toHtml c
    toHtmlRaw (TaskComment c) = toHtmlRaw c
