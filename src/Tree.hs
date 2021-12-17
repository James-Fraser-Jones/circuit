module Tree(Tree, Tree.read, insert, replace, delete, top) where

import Data.Map (Map)
import qualified Data.Map as Map

------------------------------------------------------------------------
--Memory allocation

data Alloc k = Alloc {next :: k, recycle :: [k]}

malloc :: (Ord k, Enum k) => Alloc k -> (k, Alloc k)
malloc (Alloc n []) = (n, Alloc (succ n) [])
malloc (Alloc n (r:rs)) = (r, Alloc n rs)

free :: (Ord k, Enum k) => k -> Alloc k -> Alloc k
free k (Alloc n rs) = Alloc n (k:rs)

------------------------------------------------------------------------
--Trees

data Tree n k = Tree {dict :: Map k (n k), top' :: k, alloc :: Alloc k}

top :: Tree n k -> k
top = top'

read :: (Ord k, Enum k) => k -> Tree n k -> n k
read k t = dict t Map.! k

insert :: (Ord k, Enum k) => n k -> Tree n k -> Tree n k
insert n (Tree d t a) = Tree d' t a'
  where (k, a') = malloc a
        d' = Map.insert k n d

replace :: (Ord k, Enum k) => k -> n k -> Tree n k -> Tree n k
replace k n (Tree d t a) = Tree d' t a 
  where d' = Map.update (const $ Just n) k d

delete :: (Ord k, Enum k) => k -> k -> Tree n k -> Tree n k
delete k kTop (Tree d t a) = Tree d' t' a'
  where a' = free k a
        d' = Map.delete k d
        t' = if k == t then kTop else t