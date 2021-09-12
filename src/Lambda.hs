module Lambda(normalizeLambda) where

import Types
import Utils(bracket)

import Data.Set (Set)
import qualified Data.Set as Set

normalizeLambda :: Lambda -> [Lambda]
normalizeLambda b = b : maybe [] normalizeLambda (reduceNormal b)

---------------------------------------------------------------

instance Show Lambda where
    show l = case l of
        Lam s l -> "\\" <> s <> "." <> show l
        App a b -> (if isLam a then bracket else id) (show a) <> " " <> (if isLam b || isApp b then bracket else id) (show b)
        Var s -> s
        Con s -> s
        Qte -> "#Quote"

isLam :: Lambda -> Bool
isLam l = case l of
    Lam _ _ -> True
    _ -> False

isApp :: Lambda -> Bool
isApp l = case l of
    App _ _ -> True
    _ -> False

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
reduceNormal (App (Lam s t1) t2) = Just $ sub s t2 t1    --reduce outer before inner
reduceNormal (App t1 t2) = case reduceNormal t1 of       --reduce left values before right ones
        Just t' -> Just $ App t' t2
        Nothing -> App t1 <$> reduceNormal t2
reduceNormal (Lam s t) = Lam s <$> reduceNormal t
reduceNormal _ = Nothing

---------------------------------------------------------------
--Quotation and Interpretation

{-
Quoting Function:
quote (\x.T) = \a.\b.\c.\d.\e. a (\x.quote T)
quote (T U)  = \a.\b.\c.\d.\e. b (quote T) (quote U)
quote x      = \a.\b.\c.\d.\e. c x
quote s      = \a.\b.\c.\d.\e. d s
quote #Quote = \a.\b.\c.\d.\e. e
-}
quote :: Lambda -> Lambda
quote b = undefined

-- wrap :: Brujin -> Brujin
-- wrap = foldr (.) id $ replicate 3 BLam