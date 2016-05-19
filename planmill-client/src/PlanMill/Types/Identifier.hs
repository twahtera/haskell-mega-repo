{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Identifier (
    Identifier(..),
    HasIdentifier(..),
    ) where

import PlanMill.Internal.Prelude
import Prelude                   ()

import Control.Lens (Lens')

-- | Tagged identifier
newtype Identifier a = Ident Word64
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

deriveGeneric ''Identifier

instance NFData (Identifier a)
instance Hashable (Identifier a)
instance AnsiPretty (Identifier a)
instance Binary (Identifier a)
instance HasStructuralInfo (Identifier a) where structuralInfo = sopStructuralInfo
instance HasSemanticVersion (Identifier a)

instance FromJSON (Identifier a) where
    parseJSON = fmap Ident . parseJSON
instance ToJSON (Identifier a) where
    toJSON (Ident i) = toJSON i

-- | Identities with identifier.
class HasIdentifier entity super | entity -> super where
    identifier :: Lens' entity (Identifier super)
