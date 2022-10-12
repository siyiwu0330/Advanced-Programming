module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E


exprN :: Int -> Gen Expr
exprN 0 = fmap Const (arbitrary :: Gen Int)
exprN n = oneof
   [ 
        fmap Const (arbitrary :: Gen Int)
      , fmap Var (arbitrary :: Gen String)
      , do  x <- exprN (n `div` 2)
            y <- exprN (n `div` 2)
            return $ Oper Add x y
      , do  x <- exprN (n `div` 2)
            y <- exprN (n `div` 2)
            return $ Oper Minus x y
      , do  x <- exprN (n `div` 2)
            y <- exprN (n `div` 2)
            return $ Oper Times x y
      , do  x <- exprN (n `div` 2)
            y <- exprN (n `div` 2)
            return $ Let (arbitrary :: Gen String) x y
   ]

instance Arbitrary Expr where
   arbitrary = sized exprN

prop_eval_add :: Expr -> Property
prop_eval_add x y = eval (Add x y) == eval (Add y x)

prop_eval_minus :: Expr -> Property
prop_eval_minus x y z = eval(Minus x (Minus y z)) === eval(Minus (Minus x y) z)

prop_eval_times :: Expr -> Property
prop_eval_times x y = eval (Times x y) == eval (Times y x)



prop_eval_simplify :: Expr -> Property
prop_eval_simplify E.evalTop(E.simplify x) === E.evalTop(x)
