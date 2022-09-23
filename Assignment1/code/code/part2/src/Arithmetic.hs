-- This is a skeleton file for you to edit

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

showExp :: Exp -> String
showExp (Cst x)
  | x < 0 = "(" ++ show x ++ ")"
  | otherwise = show x
showExp (Add x y) = "(" ++ showExp x ++ "+" ++ showExp y ++ ")"
showExp (Sub x y) = "(" ++ showExp x ++ "-" ++ showExp y  ++ ")"
showExp (Mul x y) = "(" ++ showExp x ++ "*" ++ showExp y  ++ ")"
showExp (Div x y) = "(" ++ showExp x ++ "`div`" ++ showExp y  ++ ")"
showExp (Pow x y) = "(" ++ showExp x ++ "^" ++ showExp y ++ ")"
showExp _ = error "Wrong Expression"

evalSimple :: Exp -> Integer
evalSimple (Cst x) = x
evalSimple (Add x y) = evalSimple x + evalSimple y
evalSimple (Sub x y) = evalSimple x - evalSimple y
evalSimple (Mul x y) = evalSimple x * evalSimple y
evalSimple (Div x y) = evalSimple x `div` evalSimple y
evalSimple (Pow x y) 
  | (evalSimple y) < 0 = error "Negative Exponent"
  | (evalSimple y) == 0 = (evalSimple x) * 0 + 1
  | otherwise          = evalSimple x ^ evalSimple y


extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r = \r' -> if r' == v then Just n else r r'

evalFull :: Exp -> Env -> Integer
evalFull (If e0 e1 e2) r = if evalFull e0 r /= 0 then evalFull e1 r else evalFull e2 r
evalFull (Let v e0 e1) r = evalFull e1 (extendEnv v (evalFull e0 r) r)
evalFull (Sum v e0 e1 e2) r
  | evalFull e0 r > evalFull e1 r = 0
  | evalFull e0 r < evalFull e1 r = evalFull (Let v e0 e2) r + evalFull (Sum v (Add e0 (Cst 1)) e1 e2) r
  | otherwise                     = evalFull (Let v e1 e2) r
evalFull (Cst x) _ = x
evalFull (Var x) r = if r x == Nothing then error "Undefined Variable" else ((\(Just x) -> x) (r x))
evalFull (Add x y) r = evalFull x r + evalFull y r
evalFull (Sub x y) r = evalFull x r - evalFull y r
evalFull (Mul x y) r = evalFull x r * evalFull y r
evalFull (Div x y) r = evalFull x r `div` evalFull y r
evalFull (Pow x y) r
  | evalFull y r < 0 = error "Nagative Exponent"
  | evalFull y r == 0 = (evalFull x r) * 0 + 1
  | otherwise        = evalFull x r ^ evalFull y r

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst i) env  = Right i 
evalErr (Add expl expr) env = eAdd (evalErr expl env)  (evalErr expr env)
evalErr (Sub expl expr) env = eSub (evalErr expl env)  (evalErr expr env)
evalErr (Mul expl expr) env = eMul (evalErr expl env)  (evalErr expr env)	
evalErr (Div expl expr) env 
	| (evalErr expr env) == Right 0 = Left EDivZero
	| otherwise = eDiv (evalErr expl env)  (evalErr expr env)
evalErr (Pow expl expr) env 
	|  eCompare (evalErr expr env) (Right 0) == LT = Left ENegPower
	| otherwise = ePow (evalErr expl env) (evalErr expr env)	
evalErr If {test=exp0, yes=exp1, no=exp2} env = eIf (evalErr exp0 env)  (evalErr exp1 env) (evalErr exp2 env)  
evalErr (Var str) env = eVar str env
--
evalErr Let {var = str, def= exp0, body=exp1} env = eLet (evalErr exp0 env) (evalErr exp1 (extendEnv str ((\(Right i)->i) (evalErr exp0 env)) env))
--
evalErr Sum {var = str, from = exp0, to = exp1, body = exp2} env
	| eCompare (evalErr exp0 env) (evalErr exp1 env) == GT = Right 0
	| eCompare (evalErr exp0 env) (evalErr exp1 env) == EQ = eSum (evalErr exp0 env) (evalErr exp1 env) (evalErr exp2 (extendEnv str ((\(Right i)->i) (evalErr exp0 env)) env))
	| eCompare (evalErr exp0 env) (evalErr exp1 env) == LT = eAdd (eSum (evalErr exp0 env) (evalErr exp1 env) (evalErr exp2 (extendEnv str ((\(Right i)->i) (evalErr exp0 env)) env)))  (evalErr Sum {var = str, from = (Add (Cst 1) exp0), to = exp1, body = exp2} env)
--

eAdd :: Either ArithError Integer -> Either ArithError Integer -> Either ArithError Integer
eAdd (Right i1) (Right i2) = Right (i1+i2)
eAdd (Left a1) _ = Left a1
eAdd _ (Left a2) = Left a2

eSub :: Either ArithError Integer -> Either ArithError Integer -> Either ArithError Integer
eSub (Right i1) (Right i2) = Right (i1-i2)
eSub (Left a1) _ = Left a1
eSub _ (Left a2) = Left a2

eMul :: Either ArithError Integer -> Either ArithError Integer -> Either ArithError Integer
eMul (Right i1) (Right i2) = Right (i1*i2)
eMul (Left a1) _ = Left a1
eMul _ (Left a2) = Left a2

eDiv :: Either ArithError Integer -> Either ArithError Integer -> Either ArithError Integer
eDiv (Right i1) (Right i2) = Right (i1 `div` i2)
eDiv (Left a1) _ = Left a1
eDiv _ (Left a2) = Left a2

ePow :: Either ArithError Integer -> Either ArithError Integer -> Either ArithError Integer
ePow (Right i1) (Right i2) = Right (i1^i2)
ePow (Left a1) _ = Left a1
ePow _ (Left a2) = Left a2

eIf :: Either ArithError Integer -> Either ArithError Integer -> Either ArithError Integer -> Either ArithError Integer
eIf (Right i1) (Right i2) (Left a3) = if i1 /= 0 then (Right i2) else (Right i2)
eIf (Right i1) (Left a2) (Right i3) = if i1 == 0 then (Right i3) else (Right i3)
eIf (Right i1) (Right i2) (Right i3) = if i1 == 0 then (Right i3) else (Right i2)
eIf (Left a1) _ _ = Left a1
eIf _ (Left a2) _ = Left a2
eIf _ _ (Left a3) = Left a3

eVar :: VName -> Env -> Either ArithError Integer
eVar n env
	| env n == Nothing = Left (EBadVar n)
	| otherwise = Right ((\(Just a)->a) (env n))


eLet ::  Either ArithError Integer -> Either ArithError Integer -> Either ArithError Integer
eLet  (Right i1) (Right i2) =  Right i2
eLet  (Left a1) _ =  Left a1
eLet  _ (Left a2) =  Left a2

eSum :: Either ArithError Integer -> Either ArithError Integer -> Either ArithError Integer -> Either ArithError Integer
eSum (Right i1) (Right i2) (Right i3) = Right i3
eSum (Left a1) _ _ =  Left a1
eSum _ (Left a2) _ =  Left a2
eSum _ _ (Left a3) =  Left a3

eCompare :: Either ArithError Integer -> Either ArithError Integer -> Ordering
eCompare (Right i1) (Right i2)
	| i1>i2 = GT
	| i1<i2 = LT
	| i1==i2 =EQ
eCompare _ _ = EQ

-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
