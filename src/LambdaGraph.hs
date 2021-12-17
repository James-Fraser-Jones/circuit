module LambdaGraph() where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

----------------------------------------------------------------------

--a graph is a hashmap with associated metadata to allow constant time insert at first available key, plus the index of the top-level node
data Graph' a = Graph' {map :: Map a (Node a), next :: a, recycle :: [a], top :: a}

--if k == Nothing, use first available key
insert :: Maybe a -> Node a -> Graph' a -> Graph' a
insert k n g = undefined

read :: a -> Graph' a -> Node a
read k g = undefined

--this should throw an error if we attempt to delete a non-existant key, otherwise recycle could end up with duplicates
delete :: a -> Graph' a -> Graph' a
delete k g = undefined

----------------------------------------------------------------------

type Graph a = Map a (Node a)

--unidirectional
-- data Node a = Var {bin :: a}
--             | App {dat :: a, fun :: a}
--             | Bin {bod :: a}

--bidirectional
data Node a = Var {up :: a, bin :: a}
            | App {up :: a, datum :: a, func :: a}
            | Bin {up :: a, body :: a, vars :: [a]}

type Term a = a

--eliminator for Terms
term :: Ord a => Graph a -> (a -> b) -> (b -> b -> b) -> (b -> [a] -> b) -> Term a -> b
term g v a b t = case g Map.! t of
  Var _ vb -> v vb
  App _ ad af -> a (term g v a b ad) (term g v a b af)
  Bin _ bb bv -> b (term g v a b bb) bv

tSize :: Ord a => Graph a -> Term a -> Int
tSize g = term g (const 1) (+) const

tVars :: Ord a => Graph a -> Term a -> Set a
tVars g = term g Set.singleton Set.union const

tBound :: Ord a => Graph a -> Term a -> Set a
tBound g = term g (const Set.empty) Set.union (\s ls -> s `Set.union` Set.fromList ls)

tFree :: Ord a => Graph a -> Term a -> Set a
tFree g t = tVars g t Set.\\ tBound g t