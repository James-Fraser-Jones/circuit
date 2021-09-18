module Tree() where --experiments with "Tree Calculus by Barry Jay"

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
--Parsing Tree Expressions

extract :: Either String Tree -> Tree
extract = either (error "Error: Bad Tree") id

parseTree :: String -> Either String Tree
parseTree s = finish s $ stripWhitespace expr

expr :: Parser Tree
expr = term `chainl1` attach

term :: Parser Tree
term = node <|> con <|> parens expr

attach :: Parser (Tree -> Tree -> Tree)
attach = return Attach

node :: Parser Tree
node = do
    char '^'
    spaces
    return Node

con :: Parser Tree
con = do
    s <- oneOf ['a'..'z']
    spaces
    return $ TCon $ pure s

---------------------------------------------------------------
--Printing Tree Expressions

instance Show Tree where
    show Node = "^"
    show (TCon s) = s
    show (Attach t u) = show t <> (if isAttach u then bracket else id) (show u)

isAttach :: Tree -> Bool
isAttach (Attach _ _) = True
isAttach _ = False

---------------------------------------------------------------
--Reducing Tree Expressions

reduce :: Tree -> Maybe Tree
reduce (Attach (Attach (Attach Node Node) y) z) = Just y
reduce (Attach (Attach (Attach Node (Attach Node x)) y) z) = Just $ Attach (Attach y z) (Attach x z)
reduce (Attach (Attach (Attach Node (Attach (Attach Node w) x)) y) z) = Just $ Attach (Attach z w) x
reduce _ = Nothing

