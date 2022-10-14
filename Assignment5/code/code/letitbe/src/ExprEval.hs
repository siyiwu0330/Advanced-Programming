module ExprEval where

import ExprAst
import qualified Data.Map.Strict as M
import Data.Map(Map)

type Env = Map String Int

oper :: Op -> (Int -> Int -> Int)
oper Plus = (+)
oper Minus = (-)
oper Times = (*)

eval :: Expr -> Env -> Either String Int
eval (Const n) _ = return n
eval (Oper op x y) env = (oper op) <$> eval x env <*> eval y env
eval (Var v) env = case M.lookup v env of
                     Nothing -> Left ("Unknown identifier: "++v)
                     Just val -> return val
eval (Let v e body) env = do
  val <- eval e env
  eval body $ M.insert v val env

evalTop e = eval e M.empty

simplify e =
  case e of
    Oper Plus (Const 0) e2 -> e2
    Oper Plus e1 (Const 0) -> e1
    Oper Plus (Const c1) (Const c2) -> Const(c1+c2)
    Oper Minus e1 (Const 0) -> e1
    Oper Minus (Const c1) (Const c2) -> Const(c1-c2)
    Oper Times (Const 1) e2 -> e2
    Oper Times (Const 0) _ -> Const(0)
    Oper Times e1 (Const 1) -> e1
    Oper Times _ (Const 0) -> Const(0)
    Oper Times (Const c1) (Const c2) -> Const(c1*c2)
    Oper op e1 e2 -> Oper op (simplify e1) (simplify e2)
    Let v e body -> case findVarUsed v body of
      True -> Let v (simplify e) (simplify body)
      False ->  simplify body
    _ -> e


findVarUsed:: String->Expr->Bool
findVarUsed str e = case e of
  Const _ -> False
  Var name-> if name == str
    then
      True
    else 
      False
  Oper _ e1 e2 -> (findVarUsed str e1)||(findVarUsed str e2)
  Let _ e1 e2 ->(findVarUsed str e1)||(findVarUsed str e2)
