import Prelude ()
import Futurice.Prelude
import Data.Aeson
import Test.Tasty
import Test.Tasty.QuickCheck

import Futurice.App.Checklist.Command 

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "QuickCheck"
    [ testProperty "Command aeson roundtrip" commandRoundtrip
    ]

commandRoundtrip :: Command Identity -> Property
commandRoundtrip cmd = Right cmd === eitherDecode (encode cmd)
