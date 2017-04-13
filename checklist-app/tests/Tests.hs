{-# LANGUAGE FlexibleContexts #-}
import Prelude ()
import Futurice.Prelude
import Data.Aeson
import Test.Tasty
import Test.Tasty.QuickCheck

import Futurice.App.Checklist.Command
import Futurice.App.Checklist.Types.TaskAppliance
import Futurice.App.Checklist.Types.Tribe

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = withTestValidTribes $ testGroup "QuickCheck"
    [ testProperty "TaskAppliance roundtrip" taskApplianceRoundtrip
    , testProperty "Command aeson roundtrip" commandRoundtrip
    ]

taskApplianceRoundtrip :: TaskAppliance -> Property
taskApplianceRoundtrip ta =
    Right ta === parseTaskAppliance (prettyTaskAppliance ta)

commandRoundtrip :: HasValidTribes => Command Identity -> Property
commandRoundtrip cmd = Right cmd === eitherDecode (encode cmd)
