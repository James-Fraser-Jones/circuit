module Types where

data Lambda = Lam String Lambda
            | App Lambda Lambda
            | Var String
            | Con String
            | Qte

data Brujin = BLam Brujin
            | BApp Brujin Brujin
            | BInd Int
            | BCon String

newtype BrujinContext = BrujinContext { unContext :: [(String, Int)] }

type Symbol = Char

data Circuit = Circuit { grid :: [[Symbol]], size :: (Int, Int), indices :: [(Int, Int)] }