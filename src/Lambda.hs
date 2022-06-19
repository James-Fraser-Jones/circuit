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

--we require that l is a combinator, we intend to strip the *minimum* amount from the outer structure *without* stripping nothing
-- step_reify :: Lambda -> Lambda
-- step_reify l = case l of
--     (Var x) -> --var is impossible at the outer layer since l is a combinator

--   (App t u) -> --app *could* be safely pulled apart and t u passed to a continuation but
--                --actually if t is (or evaluates to) a lambda abstraction then (t u) can be reduced
--                --so this must be checked *before* pulling them apart (else confluence is broken)
--                --so, essentially, if in the process of being reduced to a normal form, t does not become
--                --an abstraction at the top level, then this is safe to pull apart
--                --fortunately, normal order evaluation always starts at the top level anyway so it's quite concievable
--                --that there will be lots of applications looking like (x T) etc..

--   (Lam s t) -> --in this case, if t happens to be a combinator then we can easily return it (as a special combinator case inside the abstraction case)
--                --however it is likely that x is free in t
--                --in this case, it is important for us to descend in order to case on all the free x variables
--                --this may require us to descend below more abstractions, increasing the potential number of free variables we have to reach
--                --in the worst case, we will have no opportunity to cleanly isolate a sub-combinator and return it with the special case
--                --even in circumstances where there *are* sub-combinators, in certain circumstances (such as those combinators acting on free variables)
--                --we must evaluate these until no more computations are acting on free variables
--                --if, instead, combinators are being passed *as arguments* then they can be cased on
--                --as a reminder, the reason we must touch all free variables with reify is because otherwise we lose information that we can't
--                --get back since we will have reified the binder but not the corresponding variables and they can't be seperated in the same way
--                --as subcombinators

--         Rei -> --extremely simple case just pass correct continuation 

--I think best idea is to try and figure out how exactly fix eval works
--(like why doesn't "fix eval (#R (\a b c d e -> c (\x -> x)))") give the wrong result and try to eval what looks like
--a bunch of continuations!?!?
--From there, we can try and define program equality as a lambda term that uses reify on both arguments and performs the
--necessary checks (including testing bound variables by substituting numerals and performing numerical equality checks)
--then try carrying on with step_reify