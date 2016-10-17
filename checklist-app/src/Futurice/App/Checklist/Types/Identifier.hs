{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
-- TODO: remove no-orphans
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.Checklist.Types.Identifier (
    Identifier (..),
    identifierToText,
    HasIdentifier (..),
    ) where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.UUID as UUID

newtype Identifier a = Identifier UUID
    deriving (Eq, Ord, Show, Typeable, Generic)

identifierToText :: Identifier a -> Text
identifierToText (Identifier u) = UUID.toText u

instance Arbitrary (Identifier a) where
    arbitrary = Identifier <$> arbitrary

instance HasUUID (Identifier a) where
    uuid = lens (\(Identifier u) -> u) (\_ u -> Identifier u)

-------------------------------------------------------------------------------
-- HasIdentifier
-------------------------------------------------------------------------------

-- | TODO: evaluate if we really need this
class HasIdentifier entity ident | entity -> ident where
    identifier :: Lens' entity (Identifier ident)

instance HasIdentifier (Identifier e) e where
    identifier = id
