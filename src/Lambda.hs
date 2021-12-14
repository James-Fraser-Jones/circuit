module Lambda(lambda, parseLambda, normalizeLambda) where

import Types
import Utils
import Parser

import Data.Either
import Data.List
import Control.Applicative
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

lambda :: Int -> Int -> String -> String
lambda i n s = either id (format i n normalizeLambda) (parseLambda s)

---------------------------------------------------------------
--Parsing Lambda Expressions

parseLambda :: String -> Either String Lambda
parseLambda s = finish s $ strip expr

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
    vs <- vars
    spaces
    ((() <$ char '.') <|> (() <$ string "->"))
    spaces
    e <- expr
    return $ foldr ($) e $ fmap Lam vs

var :: Parser String
var = iden

vars :: Parser [String]
vars = do
    v <- var
    vs <- many $ spaces *> var 
    return $ v : vs

con :: Parser Lambda
con = do
    c <- oneOf $ ['A'..'Z']
    s <- iden
    return $ Con $ c:s

qte :: Parser Lambda
qte = do
    string "#Quote"
    return Qte

---------------------------------------------------------------
--Printing Lambda Expressions

instance Show Lambda where
    show l = case l of
        Lam s t -> collect [s] t
        App a b -> (if isLam a then bracket else id) (show a) <> " " <> (if isLam b || isApp b then bracket else id) (show b)
        Var s -> s
        Con s -> s
        Qte -> "#Quote"

collect :: [String] -> Lambda -> String
collect names t = case t of
    (Lam s t') -> collect (names <> pure s) t'
    t' -> "\\" <> join (intersperse " " names) <> " -> " <> show t'  

isLam :: Lambda -> Bool
isLam l = case l of
    Lam _ _ -> True
    _ -> False

isApp :: Lambda -> Bool
isApp l = case l of
    App _ _ -> True
    _ -> False

---------------------------------------------------------------
--Reducing Lambda Expressions

--get all of the free variables of a lambda expression
free :: Lambda -> Set String
free (Lam v l) = Set.difference (free l) (Set.singleton v)
free (App l1 l2) = Set.union (free l1) (free l2)
free (Var v) = Set.singleton v
free c = Set.empty
    
fresh :: Set String -> String -> String
fresh exclude s = if not (Set.member s' exclude) then s' else fresh exclude s'
    where s' = s <> "\'"

--http://www.cs.nott.ac.uk/~psznhn/G54FOP/LectureNotes/lecture11-9up.pdf
sub :: String -> Lambda -> Lambda -> Lambda
sub x t (Lam y t') =
    if y == x then
        Lam y t'
    else if not (Set.member y (free t)) then
        Lam y (sub x t t')
    else
        Lam z (sub x t t'')
            where t'' = sub y (Var z) t'
                  z = fresh (Set.unions [(Set.singleton x), (free t), (free t')]) y
sub x t (App t1 t2) = App (sub x t t1) (sub x t t2)
sub x t (Var s) = if s == x then t else (Var s)
sub x t c = c

reduceNormal :: Lambda -> Maybe Lambda
reduceNormal (App Qte t) = Just $ quote t
reduceNormal (App (Lam s t1) t2) = Just $ sub s t2 t1    --reduce outer before inner
reduceNormal (App t1 t2) = case reduceNormal t1 of       --reduce left values before right ones
        Just t' -> Just $ App t' t2
        Nothing -> App t1 <$> reduceNormal t2
reduceNormal (Lam s t) = Lam s <$> reduceNormal t
reduceNormal _ = Nothing

normalizeLambda :: Lambda -> [Lambda]
normalizeLambda b = b : maybe [] normalizeLambda (reduceNormal b)

---------------------------------------------------------------
--Quotation and Interpretation

{-
Quote Wrapping Functions:
quote x      = \a b c d e -> a x
quote (T U)  = \a b c d e -> b (quote T) (quote U)
quote (\x.T) = \a b c d e -> c (\x.quote T)
quote s      = \a b c d e -> d s
quote #Quote = \a b c d e -> e
-}
quote :: Lambda -> Lambda --THIS CAN ACCIDENTALLY CAPTURE VARIABLES IN ITS CURRENT ITERATION
quote t = fromRight (error $ "Quoting Error: Failed to parse expression \"" <> show t <> "\"") $ parseLambda $ case t of
    (Var x) -> "\\a b c d e -> a " <> x
    (App t u) -> "\\a b c d e -> b (" <> show (quote t) <> ") (" <> show (quote u) <> ")"  
    (Lam s t) -> "\\a b c d e -> c (\\" <> s <> " -> " <> show (quote t) <> ")"
    (Con s) -> "\\a b c d e -> d " <> s
    Qte -> "\\a b c d e -> e"