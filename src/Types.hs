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
            | BQte

data Tree = Node
          | Attach Tree Tree
          | TCon String

data LunaOp = LImp 
            | LAnd 
            | LOr
            | LApp

data Luna = LVar String
          | LBin String
          | LOp LunaOp Luna Luna

data Circuit = Circuit { grid :: [[Symbol]], size :: (Int, Int), indices :: [(Int, Int)] }

newtype Parser a = Parser { parse :: String -> Either String (String, a) }

newtype BrujinContext = BrujinContext { unContext :: [(String, Int)] }

type Symbol = Char

