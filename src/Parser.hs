module Parser where

import Control.Applicative
import Control.Monad

import Types

---------------------------------------------------------------
--Parsing Types and Instances

instance Functor Parser where
    fmap = (=<<) . (.) return

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Monad Parser where
    return a = Parser $ \s -> Right (s, a)
    pa >>= f = Parser $ \s -> case parse pa s of
        Right (s', a) -> parse (f a) s'
        Left err -> Left err

instance Alternative Parser where
    empty = Parser $ \s -> Left "Parse Error: Reached Alternative.Empty parser"
    pa <|> pb = Parser $ \s -> case parse pa s of
        Left err -> parse pb s
        val -> val

---------------------------------------------------------------
--Parsing Combinators

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \s -> case s of
    [] -> Left "Parse Error: \"satisfy\" parser given empty input"
    (x:xs) -> 
        if predicate x then
            Right (xs, x)
        else
            Left $ "Parse Error: \"satisfy\" parser predicate failed on character \'" <> pure x <> "\'"

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string s = case s of
    [] -> return []
    (x:xs) -> do
        x' <- char x
        xs' <- string xs
        return $ x' : xs'

lower :: Parser Char
lower = oneOf ['a'..'z']

upper :: Parser Char
upper = oneOf ['A'..'Z']

digit :: Parser Char
digit = oneOf ['0'..'9']

iden :: Parser String
iden = do 
    x <- lower
    xs <- many $ lower <|> upper <|> digit <|> char '_'
    return $ x:xs

space :: Parser Char
space = oneOf [' ','\t','\n','\r']

spaces :: Parser String
spaces = many space

spaces1 :: Parser String
spaces1 = some space

strip :: Parser a -> Parser a
strip pa = do
    spaces
    a <- pa
    spaces
    return a

parens :: Parser a -> Parser a
parens pa = do
    char '('
    a <- strip pa
    char ')'
    return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a --magic to avoid problems with left recursion
pa `chainl1` op = do 
    a <- pa
    rest a
    where 
        rest x = ( do 
            f <- op
            y <- pa
            rest (f x y)
            ) <|> return x

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
pa `chainr1` op = scan
    where
        scan = do
            x <- pa 
            rest x 
        rest x = ( do 
            f <- op
            y <- scan
            return (f x y)
            ) <|> return x

finish :: String -> Parser a -> Either String a
finish s pa = case parse pa s of
    Right ("", a) -> Right a
    Right (s, a) -> Left $ "Parse Error: Some input failed to parse \"" <> s <> "\""
    Left err -> Left err