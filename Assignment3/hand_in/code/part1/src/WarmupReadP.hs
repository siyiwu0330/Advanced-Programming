module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   E  ::= TE'
--   E' ::= "+" TE' | "-" TE' | ε
--   T  ::= num | "(" E ")" | ε

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
  -- may use instead of +++ for easier portability to Parsec

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

data Num = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" 
  deriving (Eq, Show)

parseString :: String -> Either ParseError Exp
parseString = E

E :: String -> Either ParseError Exp

T :: String -> Either ParseError Exp
T s = case s of
  Num         -> num2Int
  ("(" E ")") -> 

num2Int :: Num -> Either ParseError Exp
num2Int n
  | n == "0"  = Right 0
  | n == "1"  = Right 1
  | n == "2"  = Right 2
  | n == "3"  = Right 3
  | n == "4"  = Right 4
  | n == "5"  = Right 5
  | n == "6"  = Right 6
  | n == "7"  = Right 7
  | n == "8"  = Right 8
  | n == "9"  = Right 9
  | otherwise = Left "TError: Not a Int"