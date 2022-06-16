module Natree() where --experiments with "Tree Calculus by Barry Jay"

import Utils
import Types
import Parser

import Control.Applicative
import Data.Either

---------------------------------------------------------------
--Examples

example1 = extract $ parseTree "^^yz"
example2 = extract $ parseTree "^(^x)yz"
example3 = extract $ parseTree "^(^wx)yz"

---------------------------------------------------------------
--Parsing Natree Expressions

extract :: Either String Natree -> Natree
extract = either (error "Error: Bad Natree") id

parseTree :: String -> Either String Natree
parseTree s = finish s $ strip expr

expr :: Parser Natree
expr = term `chainl1` attach

term :: Parser Natree
term = node <|> con <|> parens expr

attach :: Parser (Natree -> Natree -> Natree)
attach = return Attach

node :: Parser Natree
node = do
    char '^'
    spaces
    return Node

con :: Parser Natree
con = do
    s <- oneOf ['a'..'z']
    spaces
    return $ TCon $ pure s

---------------------------------------------------------------
--Printing Natree Expressions

instance Show Natree where
    show Node = "^"
    show (TCon s) = s
    show (Attach t u) = show t <> (if isAttach u then bracket else id) (show u)

isAttach :: Natree -> Bool
isAttach (Attach _ _) = True
isAttach _ = False

---------------------------------------------------------------
--Reducing Natree Expressions

reduce :: Natree -> Maybe Natree
reduce (Attach (Attach (Attach Node Node) y) z) = Just y
reduce (Attach (Attach (Attach Node (Attach Node x)) y) z) = Just $ Attach (Attach y z) (Attach x z)
reduce (Attach (Attach (Attach Node (Attach (Attach Node w) x)) y) z) = Just $ Attach (Attach z w) x
reduce _ = Nothing

