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
    parseString "2!" @?=
      Left "Invalid Input",
  testCase "simple number" $
    parseString "-10086" @?=
      Right [SExp (Const (IntVal (-10086)))],
  testCase "test define & ';'" $
    parseString "x = 1; y = 2" @?=
      Right [SDef "x" (Const (IntVal 1)),SDef "y" (Const (IntVal 2))],
  testCase "test spaces" $
    parseString "    1     +      2      *      3      " @?=
      Right [SExp (Oper Plus (Const (IntVal 1)) (Oper Times (Const (IntVal 2)) (Const (IntVal 3))))],
  testCase "test lists & String" $
    parseString "[[1,2,3],['one','two','three']]" @?=
      Right [SExp (List [List [Const (IntVal 1),Const (IntVal 2),Const (IntVal 3)],List [Const (StringVal "one"),Const (StringVal "two"),Const (StringVal "three")]])] ,
  testCase "test comput1" $
    parseString "-1+23-(-456)" @?=
      Right [SExp (Oper Minus (Oper Plus (Const (IntVal (-1))) (Const (IntVal 23))) (Const (IntVal (-456))))],
  testCase "test comput2" $
    parseString "(1+2)-3*4>=5%6-7//8" @?=
      Right [SExp (Not (Oper Less (Oper Minus (Oper Plus (Const (IntVal 1)) (Const (IntVal 2))) (Oper Times (Const (IntVal 3)) (Const (IntVal 4)))) (Oper Minus (Oper Mod (Const (IntVal 5)) (Const (IntVal 6))) (Oper Div (Const (IntVal 7)) (Const (IntVal 8))))))],
  testCase "test comput3" $
    parseString "(1*(2//(3%4)))" @?=
      Right [SExp (Oper Times (Const (IntVal 1)) (Oper Div (Const (IntVal 2)) (Oper Mod (Const (IntVal 3)) (Const (IntVal 4)))))],
  testCase "test ccfor & ccif" $
    parseString "[x for y in [1,2,3] if (y != 0)]" @?=
      Right [SExp (Compr (Var "x") [CCFor "y" (List [Const (IntVal 1),Const (IntVal 2),Const (IntVal 3)]),CCIf (Not (Oper Eq (Var "y") (Const (IntVal 0))))])],
  testCase "test Eq" $
    parseString "((1<2)==(4>=3))!=False" @?=
      Right [SExp (Not (Oper Eq (Oper Eq (Oper Less (Const (IntVal 1)) (Const (IntVal 2))) (Not (Oper Less (Const (IntVal 4)) (Const (IntVal 3))))) (Const FalseVal)))],
  testCase "test string" $
    parseString "\n \t 1 \t + 1 \n; \n x \t = 1" @?=
      Right [SExp (Oper Plus (Const (IntVal 1)) (Const (IntVal 1))),SDef "x" (Const (IntVal 1))],
  testCase "test comment" $
    parseString "# test 1 \n 1 # test 2 \n" @?=
      Right [SExp (Const (IntVal 1))],
  testCase "test syntax error" $
    parseString "1(-1)" @?=
      Left "Invalid Input",
  testCase "test keyword error" $
    parseString "for = 1" @?=
      Left "Invalid Input",
  testCase "test name def1" $
    parseString "_t_est_ = 1" @?=
      Right [SDef "_t_est_" (Const (IntVal 1))],
  testCase "test name def2" $
    parseString "0Test = 1" @?=
      Left "Invalid Input",
  testCase "test number error" $
    parseString "-001" @?=
      Left "Invalid Input",
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p]
