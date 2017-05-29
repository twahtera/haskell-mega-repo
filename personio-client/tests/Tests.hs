{-# LANGUAGE FlexibleContexts #-}
import Prelude ()
import Futurice.Prelude
import Data.Aeson
import Test.Tasty
import Test.Tasty.QuickCheck

import Personio

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "QuickCheck"
    [ testProperty "Employee roundtrip" employeeAesonRoundtrip
    ]

employeeAesonRoundtrip :: Employee -> Property
employeeAesonRoundtrip e = lhs === rhs
  where
    lhs = eitherDecode (encode e)
    rhs = Right e
