import           Test.Tasty
--import Test.Tasty.SmallCheck as SC
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Data.List
import           Data.Ord

-- <http://documentup.com/feuerbach/tasty Tasty example>
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [smokeTests]

smokeTests = testGroup "Smoke Tests" $
  [helloWorldTest, secondTest]

helloWorldTest = testCase "Hello test world." $
  return ()

secondTest = testCase "Second test failing." $
  assertFailure "I'm not working."
