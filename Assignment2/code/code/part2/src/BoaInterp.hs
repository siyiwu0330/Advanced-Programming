-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp (\_ -> (Right a, mempty))
  x >>= f = Comp (\env -> case runComp x env of
                            (Left err, s1) -> (Left err, s1)
                            (Right x1, s1) -> case runComp (f x1) env of
                                                (Left err, s2) -> (Left err, s1 `mappend` s2)
                                                (Right x2, s2) -> (Right x2, s1 `mappend` s2))

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort err = Comp (\_ -> (Left err, mempty))

look :: VName -> Comp Value
look vn = Comp (\env -> case getValue vn env of
                          Nothing  -> (Left (EBadVar vn), mempty)
                          Just x   -> (Right x, mempty))

getValue :: VName -> Env -> Maybe Value
getValue _ [] = Nothing
getValue vn env
  | vn == fst (head env) = Just (snd $ head env)
  | otherwise              = getValue vn (tail env)


withBinding :: VName -> Value -> Comp a -> Comp a
withBinding vn v m = Comp (\env -> runComp m ((vn, v):env))

output :: String -> Comp ()
output s = Comp (\_ -> (Right (), [s]))

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy NoneVal        = False
truthy FalseVal       = False
truthy (IntVal 0)     = False
truthy (StringVal "") = False
truthy (ListVal [])   = False
truthy _              = True

operate :: Op -> Value -> Value -> Either String Value
operate Plus (IntVal x) (IntVal y)  = Right (IntVal (x + y))
operate Minus (IntVal x) (IntVal y) = Right (IntVal (x - y))
operate Times (IntVal x) (IntVal y) = Right (IntVal (x * y))
operate Div (IntVal x) (IntVal y)
  | IntVal y == IntVal 0          = Left "Div Zero"
  | otherwise                       = Right (IntVal (x `div` y))
operate Mod (IntVal x) (IntVal y)
  | IntVal y == IntVal 0          = Left "Mod Zero"
  | otherwise                       = Right (IntVal (x `mod` y))
operate Eq x y
  | x == y                          = Right TrueVal
  | otherwise                       = Right FalseVal
operate Less (IntVal x) (IntVal y)
  | x < y                           = Right TrueVal
  | otherwise                       = Right FalseVal
operate Greater (IntVal x) (IntVal y)
  | x > y                           = Right TrueVal
  | otherwise                       = Right FalseVal
operate In x (ListVal y)
  | x `elem` y                      = Right TrueVal
  | otherwise                       = Right FalseVal
operate _ _ _                       = Left "Operate Error"

apply :: FName -> [Value] -> Comp Value
-- "range"
apply "range" [IntVal e0]                       = apply "range" [IntVal 0, IntVal e0, IntVal 1]
apply "range" [IntVal e0, IntVal e1]            = apply "range" [IntVal e0, IntVal e1, IntVal 1]
apply "range" [_, _, IntVal 0]  = abort (EBadArg "Illegal Input: zero")
apply "range" [IntVal e0, IntVal e1, IntVal e2] = Comp (\_ -> (Right (ListVal (rangeFunc (IntVal e0) (IntVal e1) (IntVal e2))), mempty))
apply "range" _                                 = abort (EBadArg "Syntax Error: range")
-- "print"
apply "print" s = 
  do 
  {
    output (printFunc s);
    return NoneVal
  }
-- Err
apply x _ = abort (EBadFun x)

rangeFunc :: Value -> Value -> Value -> [Value]
rangeFunc (IntVal e0) (IntVal e1) (IntVal e2)
  | e0 >= e1 && e2 > 0 = []
  | e0 <= e1 && e2 < 0 = []
  | otherwise          = IntVal e0 : (rangeFunc (IntVal (e0 + e2)) (IntVal e1) (IntVal e2))
rangeFunc _ _ _        = error "Not Int"

printFunc :: [Value] -> String
printFunc [] = ""
printFunc (x:[])         = printValue x
printFunc (x:xs)         = printValue x ++ " " ++ printFunc xs

printValue :: Value -> String
printValue NoneVal       = "None"
printValue TrueVal       = "True"
printValue FalseVal      = "False"
printValue (IntVal x)    = show x
printValue (StringVal x) = x
printValue (ListVal x)   = "[" ++ getListValue x ++ "]"

getListValue :: [Value] -> String
getListValue [] = ""
getListValue (x:[])      = printValue x
getListValue (x:xs)      = printValue x ++ ", " ++ getListValue xs

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const x) = return x
eval (Var x)   = look x
eval (Oper op e0 e1) = 
  do
    {
      x <- eval e0;
      y <- eval e1;
      case operate op x y of
        (Right m) -> return m
        (Left m)  -> abort (EBadArg m)
    }
eval (Not e) = 
  do
    {
      x <- eval e;
      if truthy x
        then return FalseVal
      else 
        return TrueVal
    }
eval (Call f e)  = 
  do
    {
      x <- eval (List e);
      case x of
        (ListVal y)  -> apply f y
        _            -> error "Call Error"
    }
eval (List []) = return (ListVal [])
eval (List (x:xs)) = 
  do
    {
      y  <- eval x;
      ys <- eval (List xs);
      case ys of
        (ListVal zs) -> return (ListVal (y:zs))
        _            -> error "List Error"
    }

eval (Compr _ []) = return (ListVal [])
eval (Compr exp ((CCFor vn c):cs)) = 
  do
    {
      e <- eval c;
      case e of
        ListVal e' -> concat $ map (\x -> withBinding vn x (eval (Compr e cs))) e'
        _          -> abort (EBadArg "CCFor Error: CCFor VName (List [Exp])")
    }
eval (Compr exp ((CCIf c):cs)) = 
  do
    {
      e <- eval c;
      if truthy e
        then eval (Compr exp cs)
      else
        return (ListVal [])
    }


isListVal :: Value -> Bool
isListVal (ListVal (x:xs)) = True
isListVal _ = False

extractList :: Value -> [Value]
extractList (ListVal x) = x
extractList x = []

exec :: Program -> Comp ()
exec [] = return mempty
exec ((SDef v e):xs) =
  do
    {
      x <- eval e;
      withBinding v x (exec xs)
    }
exec ((SExp e):xs) =
  do
    {
      eval e;
      exec xs
    }

execute :: Program -> ([String], Maybe RunError)
execute x = 
  case (fst (runComp (exec x) [])) of
    (Right _) -> (snd (runComp (exec x) []), Nothing)
    (Left y)  -> (snd (runComp (exec x) []), Just y)
