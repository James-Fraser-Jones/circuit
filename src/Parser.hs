module Parser(parseLambda) where

import Control.Applicative
import Control.Monad

import Types

parseLambda :: String -> Either String Lambda
parseLambda s = finish s $ stripWhitespace expr

---------------------------------------------------------------
--Parsing Types and Instances

newtype Parser a = Parser { parse :: String -> Either String (String, a) }

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

iden :: Parser String
iden = many $ oneOf $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> ['_', '-', '\'']

char :: Char -> Parser ()
char c = do
    satisfy (c ==)
    return ()

string :: String -> Parser ()
string s = case s of
    [] -> return ()
    (x:xs) -> do
        char x
        string xs

spaces :: Parser String
spaces = many $ oneOf " \t\n\r"

stripWhitespace :: Parser a -> Parser a
stripWhitespace pa = do
    spaces
    a <- pa
    spaces
    return a

parens :: Parser a -> Parser a
parens pa = do
    char '('
    spaces
    a <- pa
    spaces
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

finish :: String -> Parser a -> Either String a
finish s pa = case parse pa s of
    Right ("", a) -> Right a
    Right (s, a) -> Left $ "Parse Error: Some input failed to parse \"" <> s <> "\""
    Left err -> Left err

---------------------------------------------------------------
--Parsing Lambda Expressions

expr :: Parser Lambda
expr = term `chainl1` app

app :: Parser (Lambda -> Lambda -> Lambda)
app = do
    spaces
    return App

term :: Parser Lambda
term = lam <|> (Var <$> var) <|> con <|> qte <|> parens expr

lam :: Parser Lambda
lam = do
    oneOf ['\\', 'λ']
    spaces
    s <- var
    spaces
    char '.'
    spaces
    e <- expr
    return $ Lam s e

var :: Parser String
var = do
    c <- oneOf $ ['a'..'z']
    s <- iden
    return $ c:s

con :: Parser Lambda
con = do
    c <- oneOf $ ['A'..'Z']
    s <- iden
    return $ Con $ c:s

qte :: Parser Lambda
qte = do
    string "#Quote"
    return Qte