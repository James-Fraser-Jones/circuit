module Luna() where

import Types
import Utils
import Parser

import Control.Applicative
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

-- data LunaOp = LImp 
--             | LAnd 
--             | LOr
--             | LApp

-- data Luna = LVar String
--           | LBin String
--           | LOp LunaOp Luna Luna

---------------------------------------------------------------
--Parsing Luna Expressions

parseLuna :: String -> Either String Luna
parseLuna s = finish s $ strip expr

opStr :: LunaOp -> String
opStr op = case op of
  LImp -> "->"
  LAnd -> "."
  LOr -> "|"
  LApp -> ""

opApp :: LunaOp -> Parser (Luna -> Luna -> Luna)
opApp op = do
  strip $ string $ opStr op
  return $ LOp op

bin :: Parser Luna
bin = do
  name <- iden
  char '\''
  return $ LBin name

var :: Parser Luna
var = do
  name <- iden
  return $ LVar name

term :: Parser Luna
term = bin <|> var <|> parens expr

expr :: Parser Luna
expr = (((term `chainl1` opApp LAnd) `chainl1` opApp LApp) `chainr1` opApp LImp) `chainr1` opApp LOr

---------------------------------------------------------------
--Printing Luna Expressions

instance Show LunaOp where
  show op = case op of
    LImp -> " -> "
    LAnd -> "."
    LOr -> " | "
    LApp -> " "
  
instance Show Luna where
  show expr = case expr of
    LVar s -> s
    LBin s -> s <> "'"
    LOp o a b -> (show' False o a) <> show o <> (show' True o b)

--provides prescedence and associativity (False = Left, True = Right)
opInfo :: LunaOp -> (Int, Bool)
opInfo op = case op of
  LAnd -> (3, False)
  LApp -> (2, False)
  LImp -> (1, True)
  LOr -> (0, True)

show' :: Bool -> LunaOp -> Luna -> String
show' isRight op expr = case expr of
  e@(LOp op' a b) ->
    let (p, a) = opInfo op
        (p', _) = opInfo op'
        f = case compare p p' of
              LT -> id
              GT -> bracket
              EQ -> if isRight == a then id else bracket
     in f $ show e
  v -> show v

---------------------------------------------------------------
--Reducing Luna Expressions

sub :: String -> Luna -> Luna -> Luna
sub = undefined

---------------------------------------------------------------
--Test Expressions

test1 :: Luna
test1 = (
    LOp LOr
      ( LOp LImp
          ( LOp LApp
              ( LOp LAnd
                  (LBin "a")
                  (LVar "b")
              )
              ( LOp LAnd
                  (LVar "c")
                  (LVar "d")
              )
          )
          ( LOp LApp
              ( LOp LAnd
                  (LVar "e")
                  (LVar "f")
              )
              ( LOp LAnd
                  (LVar "g")
                  (LVar "h")
              )
          )
      )
      ( LOp LImp
          ( LOp LApp
              ( LOp LAnd
                  (LVar "i")
                  (LVar "j")
              )
              ( LOp LAnd
                  (LVar "k")
                  (LVar "l")
              )
          )
          ( LOp LApp
              ( LOp LAnd
                  (LVar "m")
                  (LVar "n")
              )
              ( LOp LAnd
                  (LVar "o")
                  (LVar "p")
              )
          )
      )
  )

test11 :: Luna
test11 = (
    LOp LAnd
      ( LOp LApp
          ( LOp LImp
              ( LOp LOr
                  (LVar "a")
                  (LVar "b")
              )
              ( LOp LOr
                  (LVar "c")
                  (LVar "d")
              )
          )
          ( LOp LImp
              ( LOp LOr
                  (LVar "e")
                  (LVar "f")
              )
              ( LOp LOr
                  (LVar "g")
                  (LVar "h")
              )
          )
      )
      ( LOp LApp
          ( LOp LImp
              ( LOp LOr
                  (LVar "i")
                  (LVar "j")
              )
              ( LOp LOr
                  (LVar "k")
                  (LVar "l")
              )
          )
          ( LOp LImp
              ( LOp LOr
                  (LVar "m")
                  (LVar "n")
              )
              ( LOp LOr
                  (LVar "o")
                  (LVar "p")
              )
          )
      )
  )

test2 :: Luna
test2 = (
    LOp LImp
      (LVar "x")
      ( LOp LImp
          (LVar "y")
          (LVar "z")
      )
  )

test3 :: Luna
test3 = (
    LOp LImp
      ( LOp LImp
          (LVar "x")
          (LVar "y")
      )
      (LVar "z")
  )