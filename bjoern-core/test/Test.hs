import Björn.Core
import Core.IO
import Core.Position
import Control.Monad
import Test.HUnit
import System.Exit

main = do
    counts <- runTestTT allTests
    when (errors counts + failures counts > 0) (exitWith (ExitFailure 1))

allTests = TestList [
    TestLabel "IOTests" ioTests,
    TestLabel "PositionTests" positionTests
  ]