module Node() where

import Types
import Tree (Tree)
import qualified Tree as Tree

import Data.Set (Set)
import qualified Data.Set as Set

program :: Tree n k -> Term k
program = Term . Tree.top

-- data LambdaNode k = NVar {lam' :: k}
--                   | NApp {arg' :: k, func' :: k}
--                   | NLam {body' :: k, vars' :: [k]}

-- data ReshNode k = RVar {bins :: [k]}
--                 | RApp {arg :: k, func :: k}
--                 | RLun {head :: k, body :: k}
--                 | RBin {vars :: [k]}



