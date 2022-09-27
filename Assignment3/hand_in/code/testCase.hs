import Data.Char
import Control.Applicative
import Control.Monad

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

data ExprTree = Node Exp ExprTree ExprTree deriving (Eq)

instance Show ExprTree where
  show (Node exp l r) = "(" ++ show l ++ " " ++ show exp ++ " " ++ show r ++ ")"


instance Monad Parser where
    return a = Parser (\str -> Just (a, str))
    m >>=  f = Parser (\str -> case runParser m str of
                                    Nothing -> Nothing
                                    Just (a', str') -> runParser (f a') str')

instance Functor Parser where
  fmap = liftM
instance Applicative Parser where
  pure = return; (<*>) = ap

instance Alternative Parser where
  empty = Parser $ const Nothing
  pa <|> pb = Parser $ \inp ->
    case runParser pa inp of
      Nothing -> runParser pb inp
      x -> x

-- char :: Char -> Parser Char
-- char c = Parser $ \str ->
--   case str of
--     "" -> Nothing
--     (x:xs) -> if x == c 
--                 then Just (x, xs) 
--                 else Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy prd = Parser $ \str ->
  case str of
    "" -> Nothing
    (x:xs) -> if prd x 
                then Just (x, xs) 
                else Nothing

char :: Char -> Parser Char -- 改写char实现
char c = satisfy $ \x -> x == c

space :: Parser Char -- 解析空格
space = satisfy isSpace

digit :: Parser Char -- 解析十进制数字
digit = satisfy isDigit

lower :: Parser Char -- 解析小写字母
lower = satisfy isLower

upper :: Parser Char -- 解析大写字母
upper = satisfy isUpper

letter :: Parser Char -- 解析字母
letter = satisfy isLetter

alphanum :: Parser Char -- 解析字母和数字
alphanum = satisfy isAlphaNum

hexDigit :: Parser Char -- 解析十六进制数字
hexDigit = satisfy isHexDigit

octDigit :: Parser Char -- 解析八进制数字
octDigit = satisfy isOctDigit

oneOf :: String -> Parser Char
oneOf cs = satisfy (`elem` cs) -- 解析给定字符串里的其中一个Char

noneOf :: String -> Parser Char
noneOf cs = satisfy (not . (`elem` cs)) -- 解析给定字符串以外的任何Char

string :: String -> Parser String
string "" = return ""
string (x:xs) = do
  s  <- char x     -- char x解析结果绑定到s上
  ss <- string xs  -- string xs解析结果绑定到ss上
  return (s:ss)    -- 返回结构Parser

skipMany :: Parser a -> Parser ()
skipMany p = many p >> return ()

skipMany1 :: Parser a -> Parser ()
skipMany1 p = some p >> return ()

between :: Parser open -> Parser a -> Parser close -> Parser a
between o a c = o *> a <* c

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  a <- p
  as <- many (sep >> p)
  return $ a:as

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> return []

endBy :: Parser a -> Parser sep -> Parser [a]
endBy p sep = many (p <* sep)

endBy1 :: Parser a -> Parser sep -> Parser [a]
endBy1 p sep = some (p <* sep)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do
  x <- p
  trav x
  where trav x_ = do
                    f <- op
                    y <- p
                    trav (f x_ y) <|> return x_

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op x = chainl1 p op <|> return x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = do
  x <- p
  do
    f <- op
    rest <- chainr1 p op
    return (f x rest) <|> return x

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op x = chainr1 p op <|> return x

nature :: Parser Int
nature = read <$> some digit

