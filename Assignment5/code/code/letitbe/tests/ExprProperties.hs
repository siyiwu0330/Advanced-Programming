module ExprProperties where

import Test.QuickCheck

import ExprAst
import qualified ExprEval as E



exprN :: Int -> Gen Expr
exprN 0 = oneof [fmap ExprAst.Const (arbitrary::Gen Int), fmap ExprAst.Var (arbitrary::Gen String)]
exprN n = oneof 
    [fmap ExprAst.Const (arbitrary::Gen Int)
    ,fmap ExprAst.Var (arbitrary::Gen String)
    , do 
         x <- exprN (n `div` 2)
         y <- exprN (n `div` 2)
         return (ExprAst.Oper Plus x y)
   , do 
         x <- exprN (n `div` 2)
         y <- exprN (n `div` 2)
         return (ExprAst.Oper Minus x y)
   , do 
         x <- exprN (n `div` 2)
         y <- exprN (n `div` 2)
         return (ExprAst.Oper Times x y)
   , do 
         i <- fmap ExprAst.Var (arbitrary::Gen String)
         x <- exprN' (n `div` 2) --stop putting Var in expression x
         y <- exprN (n `div` 2);
         let id = getIdent i in
            return (ExprAst.Let id x y)]


exprN' :: Int -> Gen Expr
exprN' 0 = fmap ExprAst.Const (arbitrary::Gen Int)
exprN' n = oneof 
    [fmap ExprAst.Const (arbitrary::Gen Int)
    , do 
         x <- exprN' (n `div` 2)
         y <- exprN' (n `div` 2)
         return (ExprAst.Oper Plus x y)
   , do 
         x <- exprN' (n `div` 2)
         y <- exprN' (n `div` 2)
         return (ExprAst.Oper Minus x y)
   , do 
         x <- exprN' (n `div` 2)
         y <- exprN' (n `div` 2)
         return (ExprAst.Oper Times x y)
   , do 
         i <- fmap ExprAst.Var (arbitrary::Gen String)
         x <- exprN' (n `div` 2)
         y <- exprN' (n `div` 2);
         let id = getIdent i in
            return (ExprAst.Let id x y)]

instance Arbitrary Expr where
   arbitrary = sized exprN

prop_eval_simplify :: Expr -> Property
prop_eval_simplify x = E.evalTop x === E.evalTop (E.simplify x)


getIdent :: Expr -> String
getIdent (ExprAst.Var id) = id
getIdent _ = ""
