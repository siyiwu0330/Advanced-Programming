-- Skeleton file for Boa Parser.

module BoaParser (ParseError, parseString) where

import BoaAST
import Data.Char
import Control.Applicative
import Text.ParserCombinators.ReadP
-- add any other other imports you need

type ParseError = String -- you may replace this

parseString :: String -> Either ParseError Program
parseString str = case readP_to_S rpProgram str of
  [] -> Left "Parsing Error"
  _  -> case snd (last (readP_to_S rpProgram str)) of
    "" -> Right (fst (last (readP_to_S rpProgram str)))
    _  -> Left "Invalid Input"



reserved :: [String]
reserved = ["None", "True", "False", "for", "if", "in", "not"]

-- Main skeleton

rpProgram :: ReadP Program
rpProgram = rpStmts

-- Clean the spaces and comments between statements
rpStmts :: ReadP [Stmt]
rpStmts = (do 
		Text.ParserCombinators.ReadP.many space    
		Text.ParserCombinators.ReadP.many rpComment
		Text.ParserCombinators.ReadP.many space
		stm <- rpStmt
		Text.ParserCombinators.ReadP.many space
		Text.ParserCombinators.ReadP.many rpComment
		Text.ParserCombinators.ReadP.many space
		token (char ';')
		stms <- rpStmts
		return (stm:stms))
	<++ (do
		Text.ParserCombinators.ReadP.many space
		Text.ParserCombinators.ReadP.many rpComment
		Text.ParserCombinators.ReadP.many space
		stm <- rpStmt
		Text.ParserCombinators.ReadP.many space
		Text.ParserCombinators.ReadP.many rpComment
		Text.ParserCombinators.ReadP.many space
		return [stm])

rpStmt :: ReadP Stmt
rpStmt = (do
	ident <- token rpIdent
	token (char '=')
	opex <- token operExp
	return (SDef (extractIdent ident) opex))
	<++ (do
		opex <- token operExp
		return (SExp opex))


operExp :: ReadP Exp
operExp = outerOperExp

outerOperExp :: ReadP Exp
outerOperExp = (do
		token (string "not ")
		e <- operExp
		return (Not e))
	<++ (do
		token (string "not")
		token (char '(')
		e <- operExp
		token (char ')')
		return (Not e))
	<++ (do
		token (string "not")
		Text.ParserCombinators.ReadP.many1 rpComment
		e <- operExp
		return (Not e))
	<++ (do
		e1 <- intermediateOperExp
		token (string "==")
		e2 <- intermediateOperExp
		return (Oper Eq e1 e2))
	<++ (do
		e1 <- intermediateOperExp
		token (string "!=")
		e2 <- intermediateOperExp
		;return (Not (Oper Eq e1 e2)))
	<++ (do
		e1 <- intermediateOperExp
		token (char '<')
		e2 <- intermediateOperExp
		return (Oper Less e1 e2))
	<++ (do
		e1 <- intermediateOperExp
		token (string "<=")
		e2 <- intermediateOperExp
		return (Not (Oper Greater e1 e2)))
	<++ (do
		e1 <- intermediateOperExp
		token (char '>')
		e2 <- intermediateOperExp
		return (Oper Greater e1 e2))
	<++ (do
		e1 <- intermediateOperExp
		token (string ">=")
		e2 <- intermediateOperExp
		return (Not (Oper Less e1 e2)))
	<++ (do 
		e1 <- intermediateOperExp
		many1 space -- this and next lines are not equal to token (string " in ")
		token (string "in ")
		e2 <- intermediateOperExp
		return (Oper In e1 e2))
	<++ (do
		e1 <- intermediateOperExp
		token (string "not ")
		token (string "in ")
		e2 <- intermediateOperExp
		return (Not (Oper In e1 e2)))
	<++ intermediateOperExp

intermediateOperExp :: ReadP Exp
intermediateOperExp = (do
		e1 <- innerOperExp
		intermediateOperExpHelper e1)
	<++ innerOperExp

-- Using helper funciton to realize left association
intermediateOperExpHelper :: Exp -> ReadP Exp
intermediateOperExpHelper e1 = ( do
		token (char '+')
		e2 <- innerOperExp
		intermediateOperExpHelper (Oper Plus e1 e2))
	<++ ( do 
		token (char '-')
		e2 <- innerOperExp
		intermediateOperExpHelper (Oper Minus e1 e2))
	<++ return e1

innerOperExp :: ReadP Exp
innerOperExp = (do
		e1 <- concreteOperExp
		innerOperExpHelper e1)
	<++ concreteOperExp

-- Using helper funciton to realize left association
innerOperExpHelper :: Exp -> ReadP Exp
innerOperExpHelper e1 = ( do
		token (char '*')
		e2 <- concreteOperExp
		innerOperExpHelper (Oper Times e1 e2))
	<++ ( do 
		token (string "//")
		e2 <- concreteOperExp
		innerOperExpHelper (Oper Div e1 e2))
	<++ ( do 
		token (char '%')
		e2 <- concreteOperExp
		innerOperExpHelper (Oper Mod e1 e2))
	<++ return e1

concreteOperExp :: ReadP Exp
concreteOperExp = (do
		s <- skipSpaces *> subtractChar
		ss <- many1 digit <* skipSpaces;
		if head ss == '0' && length ss > 1
		then 
			fail "leading zero"
		else
			return  (BoaAST.Const (IntVal (read (s:ss) ::Int))))
	<++ (do
		s <- skipSpaces *> many1 digit <* skipSpaces;
		if head s == '0' && length s > 1
			then 
				fail "leading zero"
			else
				return  (BoaAST.Const (IntVal (read s ::Int))))
	<++ (do
		char '\''
		grossContent <- Text.ParserCombinators.ReadP.many (
			stringChar
			<|> (do
				string "\\n"
				return '\n')
			<|> (do 
				string "\\\n"
				return '\0')
			<|> (do 
				string "\\\\"
				return '\\')
			<|> (do 
				string "\\'"
				return '\'')) 
		char '\'';
		let purifiedContent = filter (`notElem` "\NUL") grossContent in
			return (BoaAST.Const (StringVal purifiedContent)))
	<++ (do 
		token (string "None")
		return (BoaAST.Const NoneVal))
	<++ (do
		token (string "True")
		return (BoaAST.Const TrueVal))
	<++ (do
		token (string "False")
		return (BoaAST.Const FalseVal))
	<++ (do
		fNameVar <- token rpIdent
		token (char '(')
		opez <- concreteOperExpz
		token (char ')')
		return (Call (extractIdent fNameVar) opez))
	<++ rpIdent
	<++ (do
		token (char '(')
		token (char '(')
		ope <- operExp
		token (char ')')
		token (char ')')
		return ope)
	<++ (do
		token (char '(')
		ope <- operExp
		token (char ')')
		return ope)
	<++ (do
		token (char '[')
		token (char '[')		
		opez <- concreteOperExpz
		token (char ']')
		token (char ']')
		return (List opez))
	<++ (do
		token (char '[')
		opez <- concreteOperExpz
		token (char ']')
		return (List opez))
	<++ (do 
		token (char '[')
		ope <- operExp
		many1 space
		token (string "for ")
		ident <- token rpIdent
		token (string "in ")
		exp <- operExp
		cz <- rpClausez
		token (char ']')
		return (Compr ope (CCFor (extractIdent ident) exp:cz)))


concreteOperExpz :: ReadP [Exp]
concreteOperExpz = concreteOperExps
	<++ (do return [])

concreteOperExps :: ReadP [Exp]
concreteOperExps = (do
		e <- operExp
		token (char ',')
		es <- concreteOperExps
		return (e:es))
	<++ (do
		e <- operExp
		return [e])

rpClausez :: ReadP [CClause]
rpClausez = (do 
		many1 space
		token (string "for ")
		ident <- token rpIdent
		token (string "in ")
		exp <- operExp
		cs <- rpClausez
		return (CCFor (extractIdent ident) exp:cs))
	<++ (do
		token (string "if ")
		exp <- operExp
		cs <- rpClausez
		return (CCIf exp:cs))
	<++ (do return [])


rpIdent :: ReadP Exp
rpIdent = do 
    c <- letter'
    cs <- letdigs ;
		if (c:cs) `notElem` reserved
            then
                return (Var (c:cs))
            else 
                fail "Cannot use reserved words as an identifier"
    where letter' = letter <|> underScore ;
           letdigs = Text.ParserCombinators.ReadP.many (alphaNum <|> underScore)

-- Helper Functions

token :: ReadP a -> ReadP a
token t = skipSpaces *> t <* skipSpaces

extractIdent :: Exp -> String
extractIdent (Var str) = str
extractIdent _         = ""

rpComment :: ReadP String
rpComment = (do
	char '#'
	comment <- munch (/= '\n')
	char '\n'
	return comment)
	<++ (do
	char '#'
	munch (/= '\n'))


space :: ReadP Char 
space = satisfy isSpace
digit :: ReadP Char 
digit = satisfy isDigit
letter :: ReadP Char 
letter = satisfy isLetter
alphaNum :: ReadP Char 
alphaNum = satisfy isAlphaNum
subtractChar :: ReadP Char
subtractChar = satisfy isSubtract
stringChar :: ReadP Char
stringChar = satisfy canPrintChar
underScore :: ReadP Char
underScore = satisfy isUnderScore
-- table :: ReadP Char
-- table = satisfy isTable



isSubtract :: Char -> Bool
isSubtract s = case s of
  '-' -> True
  _   -> False

isUnderScore :: Char -> Bool
isUnderScore u = case u of
  '_' -> True
  _   -> False

-- isTable :: Char -> Bool
-- isTable t = case t of
--   '\t' -> True
--   _   -> False

canPrintChar :: Char -> Bool
canPrintChar c = case c of 
	'\'' -> False
	'#'  -> True
	'\\' -> False	
	_    -> isPrint c
