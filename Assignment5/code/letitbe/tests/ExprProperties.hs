module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E


instance Arbitrary Expr where
   arbitrary = undefined

prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = x === x
