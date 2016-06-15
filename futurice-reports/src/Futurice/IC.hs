{-# LANGUAGE DataKinds, KindSignatures #-}
-- | Indexed container
--
-- *TODO:* Move to @futurice-prelude@
module Futurice.IC (
    IC,
    -- * List
    IList,
    nil,
    singleton,
    cons,
    append,
    ) where

import Futurice.Prelude hiding (empty)
import Futurice.Peano

newtype IC c (n :: Peano) a = IC (c a)
    deriving (Functor, Foldable, Traversable)

-------------------------------------------------------------------------------
-- List
-------------------------------------------------------------------------------

type IList = IC []

nil :: IList 'PZ a
nil = IC []

singleton :: a -> IList ('PS 'PZ) a
singleton x = IC [x]

cons :: a -> IList n a -> IList ('PS n) a
cons x (IC xs) = IC (x : xs)

append :: IList n a -> IList m a -> IList (PAdd n m) a
append (IC x) (IC y) = IC (x ++ y)
