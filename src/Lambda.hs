module Lambda(lambda, lambdaSingle, parseLambda, normalizeLambda, normalizeLambdaSingle) where

import Types
import Utils
import Parser

import Data.Maybe
import Data.Either
import Data.List
import Control.Applicative
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split(splitOn)
import Text.Read(readMaybe)

lambda :: Int -> Int -> String -> String
lambda i n s = either id (format i (n+1) normalizeLambda) (parseLambda s)

lambdaSingle :: Int -> String -> String
lambdaSingle i s = either id f (parseLambda s) where
    f l = let (terminated, l') = normalizeLambdaSingle i l 
           in show l' ++ if terminated then "" else "\n... reduction continues ..."

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
term = lam <|> (Var <$> var) <|> con <|> parens expr

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
    s <- idenUpper
    return $ Con s

---------------------------------------------------------------
--Printing Lambda Expressions

instance Show Lambda where
    show l = case l of
        Lam s t -> collect [s] t
        App a b -> (if isLam a then bracket else id) (show a) <> " " <> (if isLam b || isApp b then bracket else id) (show b)
        Var s -> s
        Con c -> c

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
free _ = Set.empty

fresh :: Set String -> String -> String
fresh exclude s = if not (Set.member s exclude) then s else fresh exclude next
  where next = intercalate "_" $ 
          if length chunks == 1 then chunks ++ pure "1" 
          else case readMaybe (last chunks) of 
            Just n -> init chunks ++ pure (show $ n + 1)
            Nothing -> chunks ++ pure "1"
        chunks = splitOn "_" s

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

isProgram :: Lambda -> Bool
isProgram l = Set.null (free l) && isNothing (reduceNormal l)

reduceNormal :: Lambda -> Maybe Lambda
reduceNormal (App (Lam s t1) t2) = Just $ sub s t2 t1    --reduce outer before inner
reduceNormal (App t1 t2) = case reduceNormal t1 of       --reduce left values before right ones
  Just t' -> Just $ App t' t2
  Nothing -> App t1 <$> reduceNormal t2
reduceNormal (Lam s t) = Lam s <$> reduceNormal t
reduceNormal _ = Nothing

normalizeLambda :: Lambda -> [Lambda]
normalizeLambda b = b : maybe [] normalizeLambda (reduceNormal b)

normalizeLambdaSingle :: Int -> Lambda -> (Bool, Lambda)
normalizeLambdaSingle (-1) l = (False, l)
normalizeLambdaSingle n l = case reduceNormal l of
    Nothing -> (True, l)
    Just l' -> normalizeLambdaSingle (n - 1) l'