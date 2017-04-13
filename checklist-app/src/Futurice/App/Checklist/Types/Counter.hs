module Futurice.App.Checklist.Types.Counter where

import Prelude ()
import Futurice.Prelude

import Futurice.App.Checklist.Types.TaskRole

data Counter = Counter !Int Int

instance Semigroup Counter where
    Counter a b <> Counter a' b' = Counter (a + a') (b + b')
instance Monoid Counter where
    mempty = Counter 0 0
    mappend = (<>)

instance NFData Counter where
    rnf (Counter _ _) = ()

data TodoCounter = TodoCounter !Counter !(PerTaskRole Counter)

instance Semigroup TodoCounter where
    TodoCounter a b <> TodoCounter a' b' = TodoCounter (a <> a') (b <> b')
instance Monoid TodoCounter where
    mempty = TodoCounter mempty mempty
    mappend = (<>)

instance NFData TodoCounter where
    rnf (TodoCounter _ _) = ()
