-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "Minimal tests" [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],
  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p]
