{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Personio.Request (
    PersonioReq(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Personio.Types

data PersonioReq a where
    PersonioEmployees :: PersonioReq [Employee]

deriving instance Eq (PersonioReq a)
deriving instance Ord (PersonioReq a)
deriving instance Show (PersonioReq a)

instance Hashable (PersonioReq a) where
    hashWithSalt salt PersonioEmployees = salt
        `hashWithSalt` (0 :: Int)
