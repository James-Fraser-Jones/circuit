module LambdaGraph() where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

----------------------------------------------------------------------

data Graph k = Graph {map :: Map k (Node k), next :: k, recycle :: [k], top :: k}

readG :: k -> Graph k -> Node k
readG k g = undefined

insertG :: Node k -> Graph k -> Graph k
insertG n g = undefined

updateG :: k -> Node k -> Graph k -> Graph k
updateG k n g = undefined

deleteG :: k -> Graph k -> Graph k
deleteG k g = undefined

----------------------------------------------------------------------

--bidirectional
data Node k = Var {bind :: k}
            | App {arg :: k, func :: k}
            | Bin {body :: k, vars :: [k]}

newtype Term k = Term {unTerm :: k}

program :: Graph k -> Term k
program = Term . top

--eliminator for Terms
term :: Ord k => Graph k -> (k -> a) -> (a -> a -> a) -> (a -> [k] -> a) -> Term k -> a
term g v a b t = case readG (unTerm t) g of
  Var vb -> v vb
  App aa af -> a (term g v a b (Term aa)) (term g v a b (Term af))
  Bin bb bv -> b (term g v a b (Term bb)) bv

tSize :: Ord k => Graph k -> Term k -> Int
tSize g = term g (const 1) (+) const

tVars :: Ord k => Graph k -> Term k -> Set k --this is wrong, singleton gives us the key of the binder for the variable
tVars g = term g Set.singleton Set.union const

tBound :: Ord k => Graph k -> Term k -> Set k
tBound g = term g (const Set.empty) Set.union (\s ls -> s `Set.union` Set.fromList ls)

tFree :: Ord k => Graph k -> Term k -> Set k
tFree g t = tVars g t Set.\\ tBound g t

dup :: Graph k -> Term k -> k -> Graph k
dup g t k = undefined

