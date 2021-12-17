module LambdaGraph() where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

type Graph a = Map a (Node a)

--unidirectional
-- data Node a = Var {bin :: a}
--             | App {left :: a, right :: a}
--             | Bin {down :: a}

--bidirectional
data Node a = Var {up :: a, bin :: a}
            | App {up :: a, left :: a, right :: a}
            | Bin {up :: a, down :: a, vars :: [a]}

type Term a = a

--eliminator for Terms
term :: Ord a => Graph a -> (a -> b) -> (b -> b -> b) -> (b -> [a] -> b) -> Term a -> b
term g v a b t = case g Map.! t of
  Var _ vb -> v vb
  App _ al ar -> a (term g v a b al) (term g v a b ar)
  Bin _ bd bv -> b (term g v a b bd) bv

tSize :: Ord a => Graph a -> Term a -> Int
tSize g = term g (const 1) (+) const

tVars :: Ord a => Graph a -> Term a -> Set a
tVars g = term g Set.singleton Set.union const

tBound :: Ord a => Graph a -> Term a -> Set a
tBound g = term g (const Set.empty) Set.union (\s ls -> s `Set.union` Set.fromList ls)

tFree :: Ord a => Graph a -> Term a -> Set a
tFree g t = tVars g t Set.\\ tBound g t