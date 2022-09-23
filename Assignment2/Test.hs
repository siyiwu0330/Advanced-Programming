-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Stubby tests"
  [testCase "crash test" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                           (Const (IntVal 2))]),
             SExp (Var "hello")]
      @?= (["4"], Just (EBadVar "hello")),
   testCase "execute misc.ast from handout" $
     do pgm <- read <$> readFile "examples/misc.ast"
        out <- readFile "examples/misc.out"
        execute pgm @?= (lines out, Nothing)]

{-

  -- Monad operations
  test1 = ("test1", runComp (look "x") [("x", IntVal 3),("y", IntVal 4)] == (Right (IntVal 3),[]) )
  test2 = ("test2", runComp (look "y") [("x", IntVal 3),("y", IntVal 4)] == (Right (IntVal 4),[]) )
  test3 = ("test3", runComp (look "z") [("x", IntVal 3),("y", IntVal 4)] == (Left (EBadVar "z"),[]))
  test4 = ("test4", runComp (output "Hello, world") [("x", IntVal 3),("y", IntVal 4)] == (Right (),["Hello, world"]) )
  test5 = ("test5", runComp (withBinding "z" (IntVal 3)  (look "z")) [("x", IntVal 3),("y", IntVal 4)] == (Right (IntVal 3),[]) )
  test6 = ("test6", runComp (abort (EBadVar "Bad Var")) [("x", IntVal 3),("y", IntVal 4)] == (Left (EBadVar "Bad Var"),[]) )                        
  -- Auxilary functions
  test7 = ("test7", truthy NoneVal == False )
  test8 = ("test8", truthy (IntVal (-1)) == True )
  test9 = ("test9", truthy (ListVal []) == False )
  test10 = ("test10", truthy (ListVal [ListVal []]) == True )
  test11 = ("test11", operate Plus (IntVal 5) (IntVal 6) == Right (IntVal 11) )
  test12 = ("test12", operate Minus (IntVal 5) (IntVal 6) == Right (IntVal (-1)) )
  test13 = ("test13", operate Times (IntVal 5) (IntVal 6) == Right (IntVal 30) )
  test14 = ("test14", operate Div (IntVal 5) (IntVal 0) == Left "Div Zero" )
  test15 = ("test15", operate Mod (IntVal 5) (IntVal 0) == Left "Mod Zero" )
  test16 = ("test16", operate Eq (IntVal 5) (IntVal 6) == Right FalseVal )
  test17 = ("test17", operate Less (IntVal 5) (IntVal 6) == Right TrueVal )
  test18 = ("test18", operate Greater (IntVal 5) (IntVal 6) == Right FalseVal )
  test19 = ("test19", operate In (IntVal 5) (ListVal [IntVal 5, IntVal 6]) == Right TrueVal )
  test20 = ("test20", operate In (IntVal 5) (ListVal [IntVal 9, IntVal 6]) == Right FalseVal )
  test21 = ("test21", runComp (apply "range"  [IntVal 5, IntVal 6]) [("x", IntVal 4),("y", IntVal 5)] == (Right (ListVal [IntVal 5]),[]) )
  test22 = ("test22", runComp (apply "range"  [IntVal 5]) [("x", IntVal 4),("y", IntVal 5)] == (Right (ListVal [IntVal 0,IntVal 1,IntVal 2,IntVal 3,IntVal 4]),[]) )
  test23 = ("test23", runComp (apply "range"  [IntVal 5, IntVal 9, IntVal 1]) [("x", IntVal 4),("y", IntVal 5)] == (Right (ListVal [IntVal 5,IntVal 6,IntVal 7,IntVal 8]),[]) )
  test24 = ("test24", runComp (apply "range"  [IntVal 5, IntVal 9, NoneVal]) [("x", IntVal 4),("y", IntVal 5)] == ((Left (EBadArg "Syntax Error: range"),[]))
  test25 = ("test25", runComp (apply "print"  [IntVal 5, IntVal 9, NoneVal]) [("x", IntVal 4),("y", IntVal 5)] == (Right NoneVal,["5 9 None"]) )
  -- Interpreter functions
  test26 = ("test26", runComp (eval (Const (StringVal "str"))) [("x", IntVal 3),("y", IntVal 4)] == (Right (StringVal "str"),[]) )
  test27 = ("test27", runComp (eval (Var "x")) [("x", IntVal 3),("y", IntVal 4)] == (Right (IntVal 3),[]) )
  test28 = ("test28", runComp (eval (Var "z")) [("x", IntVal 3),("y", IntVal 4)] == (Left (EBadVar "z"),[]) )
  test29 = ("test29", runComp (eval (Oper Times (Var "x") (Const (IntVal 6)))) [("x", IntVal 3),("y", IntVal 4)] == (Right (IntVal 18),[]) )
  test30 = ("test30", runComp (eval (Call "range" [Const (IntVal 2)])) [("x", IntVal 3),("y", IntVal 4)] == (Right (ListVal [IntVal 0,IntVal 1]),[]) )
  test31 = ("test31", runComp (eval (Call "print" [Const (IntVal 4), Const (IntVal (-4)), Const (IntVal (-1))])) [("x", IntVal 3),("y", IntVal 4)] == (Right NoneVal,["4 -4 -1"]) )
  test32 = ("test32", runComp (eval (Compr  (List [Var "z", Var "y"])   [ (CCFor "z" ( Const (ListVal [IntVal 5] ))), (CCFor "y" ( Const (ListVal [IntVal 5] )))]    ))  [("x", IntVal 4),("y", IntVal 5)] == (Right (ListVal [ListVal [IntVal 5,IntVal 5]]),[]) )
  test33 = ("test33", runComp (exec [SExp (Call "range" [Const (IntVal 2)])]) [("x", IntVal 4),("y", IntVal 5)] == (Right (),[]) )
  test34 = ("test34", runComp (exec [SDef "x" (Call "range" [Const (IntVal 2)]) ]) [("x", IntVal 4),("y", IntVal 5)] == (Right (),[]) )
  test35 = ("test35", (execute [SExp (Call "range" [Const (IntVal 2)])]) == ([],Nothing) )
  test36 = ("test36", (execute [SDef "x" (Call "range" [Const (IntVal 2)])]) == ([],Nothing) )  

-}