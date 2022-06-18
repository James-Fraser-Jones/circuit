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

lambda :: Int -> Int -> String -> String
lambda i n s = either id (format i n normalizeLambda) (parseLambda s)

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
term = lam <|> (Var <$> var) <|> metavar <|> rei <|> parens expr

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

metavar :: Parser Lambda
metavar = do
    c <- oneOf $ ['A'..'Z']
    s <- (iden <|> pure "")
    return $ Mvar $ c:s

rei :: Parser Lambda
rei = do
    string "#R"
    return Rei

---------------------------------------------------------------
--Printing Lambda Expressions

instance Show Lambda where
    show l = case l of
        Lam s t -> collect [s] t
        App a b -> (if isLam a then bracket else id) (show a) <> " " <> (if isLam b || isApp b then bracket else id) (show b)
        Var s -> s
        Rei -> "#R"
        Mvar s -> s

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
          if length chunks == 1 
            then chunks ++ pure "1" 
            else init chunks ++ pure (show (read (last chunks) + 1))
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
reduceNormal (App Rei t) = if isProgram t then Just $ reify t else case reduceNormal t of
  Just t' -> Just $ App Rei t'
  Nothing -> Nothing
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

---------------------------------------------------------------
--Reification

reify :: Lambda -> Lambda
reify l = case l of
  (Var x)   -> let f = fresh $ Set.singleton x
                in Lam (f "#a") $ Lam (f "#b") $ Lam (f "#c") $ Lam (f "#d") $ Lam (f "#e") $ App (Var (f "#a")) (Var x)
  (App t u) -> let f = fresh $ Set.union (free rt) (free ru)
                   rt = reify t
                   ru = reify u
                in Lam (f "#a") $ Lam (f "#b") $ Lam (f "#c") $ Lam (f "#d") $ Lam (f "#e") $ App (App (Var (f "#b")) rt) ru
  (Lam s t) -> let f = fresh $ (free rt)
                   rt = reify t
                in Lam (f "#a") $ Lam (f "#b") $ Lam (f "#c") $ Lam (f "#d") $ Lam (f "#e") $ App (Var (f "#c")) (Lam s rt)
  Rei       -> Lam "#a" $ Lam "#b" $ Lam "#c" $ Lam "#d" $ Lam "#e" $ Var "#d"
  (Mvar s)  -> Lam "#a" $ Lam "#b" $ Lam "#c" $ Lam "#d" $ Lam "#e" $ App (Var "#e") (Mvar s)