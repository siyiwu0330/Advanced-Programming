-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
import Text.ParserCombinators.ReadP as RP
import Data.Char
import Control.Applicative
-- add any other other imports you need

type ParseError = String -- you may replace this

intNum :: Parser Exp
intNum = token (do num <- digits
                   return (Num (read num)))
       where digits = many1(satisfy isDigit)

space :: Parser Char
space = satisfy isSpace
spaces :: Parser String
spaces = many space
spaces1 :: Parser String
spaces1 = many1 space

token :: Parser a -> Parser a
token p = spaces >> p

symbol :: String -> Parser String
symbol = token . string
schar :: Char -> Parser Char
schar = token . char

-- Name    ::= Letter Letdigs 
-- Letdigs ::= Letter Letdigs | Digit Letdigs | ε 
-- Letter  ::= "A" | ... | "Z" | "a" | ... | "z" 
-- Digit   ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" 

-- LegalNum      ::= "-" Digits | Digits
-- Digits        ::= Digit | NoneZeroDigit Digits 
-- Digit         ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" 
-- NoneZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" 

-- Comment ::=


name :: Parser String
name = token (do c <- letter
                 cs <- letdigs
                 return (c:cs))
     where letter = satisfy isLetter
           letdigs = many (letter <|> num)
           num = satisfy isDigit

data Keyword = Keyword String

reserved :: [String]
reserved = ["None", "True", "False", "for", "if", "in", "not"]

nameOrKeyword :: Parser (Either Keyword String)
nameOrKeyword  = do n <- name
                    if n `elem` reserved then return (Left(Keyword n))
                    else return (Right n)



-- Stmts     = (do Stmt
--                 return ())
--         <|> (do Stmt
--                 symbol ";"
--                 Stmts
--                 return ())
-- Stmt      = (do ident
--                 symbol "="
--                 Expr
--                 return ())
--         <|> (do Expr
--                 return ())
-- Expr      = (do numConst
--                 return ())
--         <|> (do stringConst
--                 return ())
--         <|> (do symbol "None"
--                 return ())
--         <|> (do symbol "True"
--                 return ())
--         <|> (do symbol "False"
--                 return ())
--         <|> (do ident
--                 return ())
--         <|> (do Expr
--                 Oper
--                 Expr
--                 return ())
--         <|> (do symbol "not"
--                 Expr
--                 return ())
--         <|> (do symbol "("
--                 Expr
--                 symbol ")"
--                 return ())
--         <|> (do ident
--                 symbol "("
--                 Exprz
--                 symbol ")")
--         <|> (do symbol "["
--                 Exprz
--                 symbol "]")
--         <|> (do symbol "["
--                 Expr
--                 ForClause
--                 Clausez
--                 symbol "]")
-- Oper      = (do symbol "+"
--                 return ())
--         <|> (do symbol "-"
--                 return ())
--         <|> (do symbol "*"
--                 return ())
--         <|> (do symbol "//"
--                 return ())
--         <|> (do symbol "%"
--                 return ())
--         <|> (do symbol "=="
--                 return ())
--         <|> (do symbol "!="
--                 return ())
--         <|> (do symbol "<"
--                 return ())
--         <|> (do symbol "<="
--                 return ())
--         <|> (do symbol ">"
--                 return ())
--         <|> (do symbol ">="
--                 return ())
--         <|> (do symbol "in"
--                 return ())
--         <|> (do symbol "not"
--                 symbol "in"
--                 return ())
-- ForClause =  do symbol "for"
--                 ident
--                 symbol "in"
--                 Expr
--                 return ()
-- IfClause  =  do symbol "if"
--                 Expr
--                 return ()
-- Clausez   = (return ())
--         <|> (do ForClause
--                 Clausez
--                 return ())
--         <|> (do IfClause
--                 Clausez
--                 return ())
-- Exprz     = (return ())
--         <|> (do Exprs
--                 return ())
-- Exprs     = (do Expr
--                 return ())
--         <|> (do Expr
--                 symbol ","
--                 Exprs
--                 return ())

-- ident        ::= String
-- numConst     ::= "-" digits | digits
-- stringConst  ::= "'" catchString "'"

-- ident     = num
-- numConst  = (do symbol "-"
--                 digits
--                 return ())
--         <|> (do digits
--                 return ())
-- stringConst=(do symbol "'"
--                 catchString
--                 symbol "'")


parseString :: String -> Either ParseError Program
parseString = undefined  -- define this
