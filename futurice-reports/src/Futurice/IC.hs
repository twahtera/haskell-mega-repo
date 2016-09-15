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
    replicate,
    replicateP,
    ) where

import Futurice.Prelude hiding (empty, replicate, nil)
import Futurice.Peano

import qualified Futurice.Prelude  as P

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

replicate :: forall n a. SPeanoI n => a -> IList n a
replicate = IC . P.replicate n
  where
    n = fromInteger $ sPeanoToInteger (singPeano :: SPeano n)

replicateP :: SPeanoI n => Proxy n -> a -> IList n a
replicateP _ = replicate
