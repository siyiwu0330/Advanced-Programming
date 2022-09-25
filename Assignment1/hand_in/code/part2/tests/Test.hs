-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone

tests :: [(String, Bool)]
tests = [test1, test2, test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20,test21,test22,test23,test24,test25,test26,test27,test28] where
  -- showExp tests
  test1 = ("test1", showExp (Add (Cst 1) (Sub (Cst 2) (Mul (Cst 3) (Div (Cst 4) (Pow (Cst 5) (Cst 6)))))) == "(1+(2-(3*(4`div`(5^6)))))")
  test2 = ("test2", showExp (Pow (Cst 1) (Pow (Cst 1) (Pow (Cst 1) (Pow (Cst 1) (Pow (Cst 1) (Pow (Cst 1) (Pow (Cst 1) (Pow (Cst 1) (Cst 1))))))))) == "(1^(1^(1^(1^(1^(1^(1^(1^1))))))))")
  test3 = ("test3", showExp (Add (Cst (-3)) (Cst (-4))) == "((-3)+(-4))")
  test4 = ("test4", showExp (Cst (-1)) == "(-1)")
  test5 = ("test5", showExp (Sub (Cst (-2)) (Add (Cst (-3)) (Cst (-4)))) == "((-2)-((-3)+(-4)))")
  -- evalSimple tests
  test6 = ("test6", evalSimple (Mul (Cst 1234567890) (Cst 1234567890)) == 1234567890*1234567890)                        
  test7 = ("test7", evalSimple (Cst (-1)) == -1)
  test8 = ("test8", evalSimple (Div (Cst 0) (Cst 1)) == 0)
  test9 = ("test9", evalSimple (Add (Cst 1) (Sub (Cst 2) (Mul (Cst 3) (Div (Cst 4) (Pow (Cst 5) (Cst 6)))))) == 1+(2-(3*(4`div`(5^6)))))
  test10 = ("test10", evalSimple (Pow (Cst 2) (Pow (Cst 2) (Pow (Cst 2) (Cst 2)))) == 2^(2^(2^2)))
  -- extendEnv tests
  test11 = ("test11", (extendEnv "x" 5 initEnv) "x" == Just 5)
  test12 = ("test12", (extendEnv "x" 5 (extendEnv "y" 6 initEnv)) "z" == Nothing)
  test13 = ("test13", (extendEnv "x" 5 (extendEnv "y" 6 initEnv)) "x" == Just 5)
  test14 = ("test14", (extendEnv "x" 5 (extendEnv "y" 6 initEnv)) "y" == Just 6)
  test15 = ("test15", (extendEnv "x" 5 (extendEnv "x" 6 initEnv)) "x" == Just 5)
  -- evalFull tests
  test16 = ("test16", evalFull (Cst (-1234567890987654321)) initEnv == -1234567890987654321)
  test17 = ("test17", evalFull (Add (Cst 1) (Sub (Cst 2) (Mul (Cst 3) (Div (Cst 4) (Pow (Cst 5) (Cst 6)))))) initEnv == 1+(2-(3*(4`div`(5^6)))))
  test18 = ("test18", evalFull (Let "x" (Add (Cst 3) (Cst (-10000000000000003))) (Var "x")) initEnv == -10000000000000000)
  test19 = ("test19", evalFull (Sum "x" (Let "x" (Sub (Cst 2) (Cst 1)) (Var "x")) (If (Sub (Cst 1) (Cst 1)) (Cst 1) (Cst 100)) (Var "x")) initEnv == ((1+100)*100)`div`2)
  test20 = ("test20", evalFull (Var "x") (extendEnv "x" (-1) initEnv) == -1)
  -- evalErr tests
  test21 = ("test21", evalErr (Add (Cst 1) (Sub (Cst 2) (Mul (Cst 3) (Div (Cst 4) (Pow (Cst 5) (Cst 6)))))) initEnv == Right (1+(2-(3*(4`div`(5^6))))))
  test22 = ("test22", evalErr (Let "x" (Add (Cst 3) (Var "x")) (Var "x")) initEnv == Left (EBadVar "x"))
  test23 = ("test23", evalErr (Cst (-1234567890987654321)) initEnv == Right (-1234567890987654321))
  test24 = ("test24", evalErr (Sum "x" (Cst 10000) (Cst 0) (Cst 1)) initEnv == Right 0)
  test25 = ("test25", evalErr (Sum "x" (Cst 1) (Pow (Cst 1) (Sub (Cst 1) (Cst 2))) (Sum "x" (Cst 1) (Cst 10) (Var "x"))) initEnv == Left ENegPower)
  -- pressure test
  test26 = ("test26", evalFull (Sum "x" (Let "x" (Cst 1) (Var "x")) (Sum "x" (Cst 1) (Cst 3) (Var "x")) (Pow (Var "x") (Pow (Var "x") (Var "x")))) initEnv == (1^(1^1)+2^(2^2)+3^(3^3)+4^(4^4)+5^(5^5)+6^(6^6)))
  test27 = ("test27", evalErr (Sum "x" (Cst 1) (Sum "x" (Cst 1) (Sum "x" (Cst 1) (Sum "x" (Cst 1) (Sum "x" (Cst 1) (Sum "x" (Cst 1) (Div (Sum "x" (Cst 1) (Pow (Cst 1) (Cst (-1))) (Var "x")) (Cst 0)) (Var "x")) (Var "x")) (Var "x")) (Var "x")) (Var "x")) (Var "x")) initEnv == Left EDivZero)
  test28 = ("test28", evalErr (Sum "x" (Cst 1) (Sum "x" (Cst 1) (Sum "x" (Cst 1) (Sum "x" (Cst 1) (Sum "x" (Cst 1) (Sum "x" (Cst 1) (Div (Sum "x" (Let "x" (Var "y") (Var "x")) (Pow (Cst 1) (Cst 1)) (Var "x")) (Cst 1)) (Var "x")) (Var "x")) (Var "x")) (Var "x")) (Var "x")) (Var "x")) initEnv == Left (EBadVar "y"))

main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
