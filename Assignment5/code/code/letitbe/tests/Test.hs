import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import ExprAst
import qualified ExprEval as E
import qualified ExprProperties as EP

main :: IO ()
main = defaultMain testsuite

testsuite =
  testGroup "Testing expression evaluation and simplification"
  [ testGroup "A few unit-tests"
    [ testCase "Eval: 2 + 2"
      (Right 4 @=? E.evalTop (Oper Plus (Const 2) (Const 2)))
    ,  testCase "Eval: 4 - 3"
      (Right 1 @=? E.evalTop (Oper Minus (Const 4) (Const 3)))
    ,  testCase "Eval: 3 * 4"
      (Right 12 @=? E.evalTop (Oper Times (Const 3) (Const 4)))
    , testCase "Eval: let x = 3 in x * x"
      (Right 9 @=? E.evalTop (Let "x" (Const 3)
                              (Oper Times (Var "x") (Var "x"))))
    , testCase "Simplify: x + (2 + 2)"
      (Oper Plus (Var "x") (Const 4) @=?
          E.simplify (Oper Plus (Var "x") (Oper Plus (Const 2) (Const 2))))
    , testCase "Simplify: 0 + 2"
      ((Const 2) @=?
          E.simplify  (Oper Plus (Const 0) (Const 2)))
    , testCase "Simplify: 3 - 0"
      ((Const 3) @=?
          E.simplify  (Oper Minus (Const 3) (Const 0)))
    , testCase "Simplify: x * (3 * 2)"
      (Oper Times (Var "x") (Const 6) @=?
          E.simplify (Oper Times (Var "x") (Oper Times (Const 3) (Const 2))))
    , testCase "Simplify: 0 * (0 + 2)"
      ((Const 0) @=?
          E.simplify  (Oper Times (Const 0) (Oper Plus (Const 0) (Const 2))))
    , testCase "Simplify: 1 * 6"
      ((Const 6) @=?
          E.simplify  (Oper Times (Const 1) (Const 6)))
    , testCase "Simplify: let"
      ((Const 6) @=?
          E.simplify  (Let "x" (Const 1) (Oper Times (Const 1) (Const 6))))
    ]
  , quickChecks
  ]

quickChecks =
  testGroup "QuickCheck tests"
  [ testProperty "Evaluating a simplified expression does not change its meaning"
    EP.prop_eval_simplify
  ]

