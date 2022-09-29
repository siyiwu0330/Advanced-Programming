-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative ((<|>))
-- add any other other imports you need

type Parser a = ReadP a

type ParseError = String -- you may replace this

space :: Parser Char
space = satisfy isSpace
spaces :: Parser String
spaces = many space
spaces1 :: Parser String
spaces1 = many1 space

token :: Parser a -> Parser a
token p = spaces >> p
token' :: Parser a -> Parser a
token' p = comment >> p

symbol :: String -> Parser String
symbol = token . string
schar :: Char -> Parser Char
schar = token . char


reserved :: [String]
reserved = ["None", "True", "False", "for", "if", "in", "not"]

data Keyword = Keyword String

program :: Parser Program
program = stmts

-- Stmts     = (do Stmt
--                 return ())
--         <|> (do Stmt
--                 symbol ";"
--                 Stmts
--                 return ())
stmts :: Parser [Stmt]
stmts             = (do comment
                        st <- stmt
                        comment
                        return [st])
                <++ (do comment
                        st <- stmt
                        comment
                        symbol ";"
                        comment
                        ss <- stmts
                        comment
                        return (st:ss))

-- Stmt      = (do ident
--                 symbol "="
--                 Expr
--                 return ())
--         <|> (do Expr
--                 return ())
stmt :: Parser Stmt
stmt              = (do spaces
                        id <- ident
                        symbol "="
                        er <- expr
                        return (SDef id er))
                <++ (do spaces
                        fmap (\x -> (SExp x)) expr)

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
--                 symbol ")"
--                 return ())
--         <|> (do symbol "["
--                 Exprz
--                 symbol "]"
--                 return ())
--         <|> (do symbol "["
--                 Expr
--                 ForClause
--                 Clausez
--                 symbol "]"
--                 return ())

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
expr :: Parser Exp
expr              = token oper

oper :: Parser Exp
oper              = (do e1 <- oper1
                        symbol "=="
                        e2 <- oper1
                        return (Oper Eq e1 e2))
                <++ (do e1 <- oper1
                        symbol "<"
                        e2 <- oper1
                        return (Oper Less e1 e2))
                <++ (do e1 <- oper1
                        symbol ">"
                        e2 <- oper1
                        return (Oper Greater e1 e2))
                <++ (do e1 <- oper1
                        symbol "in"
                        e2 <- oper1
                        return (Oper In e1 e2))
                <++ (do e1 <- oper1
                        symbol "!="
                        e2 <- oper1
                        return (Not (Oper Eq e1 e2)))
                <++ (do e1 <- oper1
                        symbol "<="
                        e2 <- oper1
                        return (Not (Oper Greater e1 e2)))
                <++ (do e1 <- oper1
                        symbol ">="
                        e2 <- oper1
                        return (Not (Oper Less e1 e2)))
                <++ (do e1 <- oper1
                        symbol "not"
                        symbol "in"
                        e2 <- oper1
                        return (Not (Oper In e1 e2)))
                <++ oper1

oper1 :: Parser Exp
oper1             = (do e <- oper2
                        oper1' e)
                <++ oper2

oper1' :: Exp -> Parser Exp
oper1' inval      = (do symbol "+"
                        e <- oper2
                        oper1' (Oper Plus inval e))
                <++ (do symbol "-"
                        e <- oper2
                        oper1' (Oper Minus inval e))
                <++ return inval

oper2 :: Parser Exp
oper2             = (do e <- oper3
                        oper2' e)
                <++ oper3

oper2' :: Exp -> Parser Exp
oper2' inval      = (do symbol "*"
                        e <- oper3
                        oper2' (Oper Times inval e))
                <++ (do symbol "//"
                        e <- oper3
                        oper2' (Oper Div inval e))
                <++ (do symbol "%"
                        e <- oper3
                        oper2' (Oper Mod inval e))
                <++ return inval

oper3 :: Parser Exp
oper3             = (do num <- numConst
                        return (Const (IntVal num)))
                <++ (do str <- stringConst
                        return (Const (StringVal str)))
                <++ (do symbol "None"
                        return (Const NoneVal))
                <++ (do symbol "True"
                        return (Const TrueVal))
                <++ (do symbol "False"
                        return (Const FalseVal))
                <++ (do id <- ident
                        return (Var id))
                <++ (do symbol "not"
                        er <- expr
                        return (Not er))
                <++ (do symbol "("
                        er <- expr
                        symbol ")"
                        return er)
                <++ (do id <- ident
                        symbol "("
                        ez <- exprz
                        symbol ")"
                        return (Call id ez))
                <++ (do symbol "["
                        ez <- exprz
                        symbol "]"
                        return (List ez))
                <++ (do symbol "["
                        er <- expr
                        fc <- forClause
                        cz <- clausez
                        symbol "]"
                        return (Compr er (fc:cz)))

-- ForClause =  do symbol "for"
--                 ident
--                 symbol "in"
--                 Expr
--                 return ()
forClause :: Parser CClause
forClause         =  do symbol "for"
                        id <- ident
                        symbol "in"
                        er <- expr
                        return (CCFor id er)

-- IfClause  =  do symbol "if"
--                 Expr
--                 return ()
ifClause :: Parser CClause
ifClause          =  do symbol "if"
                        er <- expr
                        return (CCIf er)

-- Clausez   = (return ())
--         <|> (do ForClause
--                 Clausez
--                 return ())
--         <|> (do IfClause
--                 Clausez
--                 return ())
clausez :: Parser [CClause]
clausez           = (return [])
                <++ (do fc <- forClause
                        cz <- clausez
                        return (fc:cz))
                <++ (do ic <- ifClause
                        cz <- clausez
                        return (ic:cz))
                <++ return []

-- Exprz     = (return ())
--         <|> (do Exprs
--                 return ())
exprz :: Parser [Exp]
exprz             = return []
                <++ exprs

-- Exprs     = (do Expr
--                 return ())
--         <|> (do Expr
--                 symbol ","
--                 Exprs
--                 return ())
exprs :: Parser [Exp]
exprs             = (do er <- expr
                        return [er])
                <++ (do er <- expr
                        symbol ","
                        es <- exprs
                        return (er:es))

-- Name    ::= Letter Letdigs 
-- Letdigs ::= Letter Letdigs | Digit Letdigs | ε 
-- Letter  ::= "A" | ... | "Z" | "a" | ... | "z" 
-- Digit   ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" 
name :: Parser String
name = token (do c <- letter
                 cs <- letdigs
                 return (c:cs))
     where letter = satisfy (\c -> isLetter c || c == '_')
           letdigs = many (letter <|> num <|> satisfy (\x -> x == '_'))
           num = satisfy isDigit
nameOrKeyword :: Parser (Either Keyword String)
nameOrKeyword  = do n <- name
                    if n `elem` reserved then return (Left(Keyword n))
                    else return (Right n)
ident :: Parser String
ident             =  token  (do nok <- nameOrKeyword
                                case nok of
                                    (Right id) -> return id
                                    (Left _)  -> fail "Keyword Error")

-- LegalNum      ::= "-" Digits | Digits
-- Digits        ::= Digit | NoneZeroDigit Digits | Digit Digits 
-- Digit         ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" 
-- NoneZeroDigit ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" 
-- numConst :: Parser Int
-- numConst          = token ( (do ds <- digits
--                                 nzd <- noneZeroDigit
--                                 return $ read $ [nzd] ++ ds)
--                         <++ (do char '-'
--                                 ds <- digits
--                                 nzd <- noneZeroDigit
--                                 return $ read $ "-" ++ [nzd] ++ ds))
--                     where digits = many (satisfy isDigit);
--                           noneZeroDigit = satisfy (\x -> x `elem` ['1'..'9'])
numConst :: Parser Int
numConst          =  do x <- option ' ' (char '-')
                        xs <- munch1 isDigit
                        if (head xs == '0') && (length xs > 1)
                            then fail "Num Error"
                        else
                            return $ read $ (x:xs)

stringConst :: Parser String
stringConst       =  do schar '\''
                        s <- isString
                        char '\''
                        return s
                where isString = many (satisfy isPrint)

comment :: Parser String
comment           = (do symbol "#"
                        com <- munch (\x -> x /= '\n')
                        symbol "\n"
                        return com)
                <++ return ""




parseString :: String -> Either ParseError Program
parseString str = case (readP_to_S program) str of
  [] -> Left "Parsing error"
  _  -> Right (fst (last ((readP_to_S program) str)))