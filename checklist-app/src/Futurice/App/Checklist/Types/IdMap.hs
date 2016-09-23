{-# LANGUAGE UndecidableInstances, TypeFamilies  #-}
module Futurice.App.Checklist.Types.IdMap (
    -- * Constructing
    IdMap,
    fromFoldable,
    -- * Keys
    keysSet,
    -- * Lens
    toIdMapOf,
    unsafeTraversal,
    ) where

import Futurice.Prelude
import Prelude ()

import Control.Lens    (Getting, toListOf, Traversal', Index, IxValue, Ixed (..), At (..))
import Data.Monoid     (Endo)
import Test.QuickCheck (Arbitrary (..), listOf1)

import qualified Data.Map as Map

import Futurice.App.Checklist.Types.Identifier

newtype IdMap a = IdMap (Map (Identifier a) a)
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Foldable IdMap where
    foldMap f (IdMap m) = foldMap f m

unIdMap :: IdMap a -> Map (Identifier a) a
unIdMap (IdMap m) = m

fromFoldable :: (HasIdentifier a a, Foldable f) => f a -> IdMap a
fromFoldable = IdMap . Map.fromList . map (\x -> (x ^. identifier, x)) . toList

keysSet :: IdMap a -> Set (Identifier a)
keysSet = Map.keysSet . unIdMap

-------------------------------------------------------------------------------
-- Lens
-------------------------------------------------------------------------------

toIdMapOf :: HasIdentifier a a => Getting (Endo [a]) s a -> s -> IdMap a
toIdMapOf l s = fromFoldable (toListOf l s)

-- | You must preserve the identifier of elements to this to be valid 'Traversal'
unsafeTraversal :: Traversal' (IdMap a) a
unsafeTraversal f (IdMap m) = IdMap <$> traverse f m

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance (HasIdentifier a a, Arbitrary a) => Arbitrary (IdMap a) where
    arbitrary = fromFoldable <$> listOf1 arbitrary
    -- TODO: shrink

type instance Index (IdMap a)   = Identifier a
type instance IxValue (IdMap a) = a

instance Ixed (IdMap a) where
    ix i f (IdMap m) = IdMap <$> ix i f m

instance At (IdMap a) where
    at i f (IdMap m) = IdMap <$> at i f m
