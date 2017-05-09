{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude ()
import Futurice.Prelude
import Data.Binary.Tagged
import GHC.TypeLits          (natVal)
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Futurice.GitHub      as GH
import qualified PlanMill.Types.Query as PM
import qualified Data.ByteString.Base16.Lazy as Base16

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "proxy-app"
    [ binaryTagTests
    ]

data BTTest where
    BTTest
        :: (HasStructuralInfo a, HasSemanticVersion a)
        => String -> Proxy a -> Int -> LazyByteString -> BTTest

binaryTagTests :: TestTree
binaryTagTests = testGroup "BinaryTagged tags" $ map mk tags
  where
    mk :: BTTest -> TestTree
    mk (BTTest name p ver h) = testProperty name $ once $
        ver === fromIntegral (natVal (versionProxy p)) .&&.
        h === Base16.encode (structuralInfoSha1ByteStringDigest (structuralInfo p))

    versionProxy :: Proxy a -> Proxy (SemanticVersion a)
    versionProxy _ = Proxy

    tags :: [BTTest]
    tags =
        [ BTTest "PlanMill.SomeResponse" (Proxy :: Proxy PM.SomeResponse)
            0 "6b3e878ac2290517d97df609f908d62a71746646"
        , BTTest "GitHub.SomeResponse" (Proxy :: Proxy GH.SomeResponse)
            0 "e299b9ec4d641300444bd32e17148546c036a3a8"
        ]
