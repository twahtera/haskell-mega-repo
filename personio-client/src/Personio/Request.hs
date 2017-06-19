{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Personio.Request (
    PersonioReq(..),
    requestDict,
    ) where

import Data.Constraint  (Dict (..))
import Futurice.Prelude
import Personio.Types
import Prelude ()

data PersonioReq a where
    PersonioEmployees   :: PersonioReq [Employee]
    PersonioValidations :: PersonioReq [EmployeeValidation]

deriving instance Eq (PersonioReq a)
deriving instance Ord (PersonioReq a)
deriving instance Show (PersonioReq a)

instance Hashable (PersonioReq a) where
    hashWithSalt salt PersonioEmployees = salt
        `hashWithSalt` (0 :: Int)
    hashWithSalt salt PersonioValidations = salt
        `hashWithSalt` (1 :: Int)

requestDict
    :: (c [Employee], c [EmployeeValidation])
    => Proxy c
    -> PersonioReq a
    -> Dict (c a)
requestDict _ PersonioEmployees = Dict
requestDict _ PersonioValidations = Dict
