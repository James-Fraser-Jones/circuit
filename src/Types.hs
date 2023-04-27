module Types where

import Data.Map (Map)
import qualified Data.Map as Map

-----------------------------------------------------------------------

data Lambda = Lam String Lambda
            | App Lambda Lambda
            | Var String
            | Con String

data Brujin = BLam Brujin
            | BApp Brujin Brujin
            | BInd Int
            | BCon String

newtype BrujinContext = BrujinContext { unContext :: [(String, Int)] }

-----------------------------------------------------------------------

data Alloc k = Alloc {next :: k, recycle :: [k]}

data Tree n k = Tree {dict :: Map k (n k), top :: k, alloc :: Alloc k}

-----------------------------------------------------------------------

data Circuit = Circuit { grid :: [[Symbol]], size :: (Int, Int), indices :: [(Int, Int)] }

type Symbol = Char

-----------------------------------------------------------------------

newtype Parser a = Parser { parse :: String -> Either String (String, a) }