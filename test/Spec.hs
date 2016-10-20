import Test.Tasty
--import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

-- <http://documentup.com/feuerbach/tasty Tasty example>
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [smokeTests]

smokeTests = testGroup "Smoke Tests"
  [ testCase "Hello test world." $
      return ()
  ]