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

iden :: Parser String
iden = many $ oneOf $ ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'] <> ['_', '\'']

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