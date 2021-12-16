import Core.IO
import Control.Monad
import Test.HUnit
import System.Exit

main = do
    counts <- runTestTT allTests
    putStrLn (showCounts counts)
    when (errors counts + failures counts > 0) (exitWith (ExitFailure 1))

allTests = TestList [
    TestLabel "IOTests" ioTests
  ]