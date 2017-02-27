import BasicPrelude
import System.Process (callProcess)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, testCase)


main :: IO ()
main = defaultMain $ testGroup "Happy path" [
        testCase "Render left prompt" $ execute ["shell", "left"],
        testCase "Render right prompt" $ execute ["shell", "right"]
    ]

-- Execute powerline binary, fail if non-zero exit code
execute :: [String] -> Assertion
execute = callProcess "powerline-hs"

