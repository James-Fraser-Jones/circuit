module Luna() where

import Types
import Utils
import Parser

import Data.List
import Data.Either
import Control.Applicative
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

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

--provides prescedence and associativity (False = Left, True = Right)
opInfo :: LunaOp -> (Int, Bool)
opInfo op = case op of
  LAnd -> (3, False)
  LApp -> (2, False)
  LImp -> (1, True)
  LOr -> (0, True)
  
instance Show Luna where
  show expr = case expr of
    LVar s -> s
    LBin s -> s <> "'"
    LOp o a b -> (show' False o a) <> show o <> (show' True o b)

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

---------------------------------------------------------------
--Parsing Bruna Expressions

parseBruna :: String -> Either String Bruna
parseBruna s = finish s $ strip bexpr

bopApp :: LunaOp -> Parser (Bruna -> Bruna -> Bruna)
bopApp op = do
  strip $ string $ opStr op
  return $ BOp op

bbin :: Parser Bruna
bbin = BBin <$ char '*'

bvar :: Parser Bruna
bvar = (BVar . read) <$> int

bterm :: Parser Bruna
bterm = bbin <|> bvar <|> parens bexpr

bexpr :: Parser Bruna
bexpr = (((bterm `chainl1` bopApp LAnd) `chainl1` bopApp LApp) `chainr1` bopApp LImp) `chainr1` bopApp LOr

---------------------------------------------------------------
--Printing Bruna Expressions

instance Show Bruna where
  show expr = case expr of
    BVar n -> show n
    BBin -> "*"
    BOp o a b -> (show'b False o a) <> show o <> (show'b True o b)

show'b :: Bool -> LunaOp -> Bruna -> String
show'b isRight op expr = case expr of
  e@(BOp op' a b) ->
    let (p, a) = opInfo op
        (p', _) = opInfo op'
        f = case compare p p' of
              LT -> id
              GT -> bracket
              EQ -> if isRight == a then id else bracket
     in f $ show e
  v -> show v

---------------------------------------------------------------
--Printing Bruna Diagrams

diagramIO :: Bruna -> IO ()
diagramIO b = putStrLn $ diagram b

diagram :: Bruna -> String
diagram expr = let (d, _, _, _) = diagram' expr in intercalate "\n" d

diagram' :: Bruna -> ([String], Int, Int, Int)
diagram' expr = case expr of
  BVar n -> ([show n], length $ show n, 1, (length $ show n) `div` 2)
  BBin -> (["*"], 1, 1, 0)
  BOp o a b -> case o of
    LApp -> diagram'' boxes1 (diagram' a) (diagram' b)
    LImp -> diagram'' boxes2 (diagram' a) (diagram' b)

{-
pad bottom of shorter block
add corner pipes to top of both blocks
merge both blocks with vertical strip
--up pipe always given in between
-}
diagram'' :: String -> ([String], Int, Int, Int) -> ([String], Int, Int, Int) -> ([String], Int, Int, Int)
diagram'' [l,h,t,r] d1@(g1, x1, y1, u1) d2@(g2, x2, y2, u2) = (n, x1 + x2 + 1, max y1 y2 + 1, x1)
  where p1 = g1 <> replicate (max 0 (y2 - y1)) (replicate x1 ' ')
        p2 = g2 <> replicate (max 0 (y1 - y2)) (replicate x2 ' ')
        c1 = (replicate u1 ' ' <> pure l <> replicate (x1 - u1 - 1) h) : p1 
        c2 = (replicate u2 h <> pure r <> replicate (x2 - u2 - 1) ' ') : p2
        v = pure t : replicate (max y1 y2) " "
        n = zipWith (<>) (zipWith (<>) c1 v) c2

boxes1 :: String
boxes1 = "┌─┴┐"

boxes2 :: String
boxes2 = "╔═╩╗"

---------------------------------------------------------------
--Reducing Bruna Expressions

--eliminator for bruna expressions
bruna :: a -> (Int -> a) -> (LunaOp -> a -> a -> a) -> Bruna -> a
bruna f g h b = case b of
  BBin -> f
  BVar n -> g n
  BOp op b1 b2 -> h op (bruna f g h b1) (bruna f g h b2)

--identity function on brunas generated using the eliminator and all constructors
idb :: Bruna -> Bruna
idb = bruna BBin BVar BOp

--number of binders in a bruna expression
stars :: Bruna -> Int
stars = bruna 1 (const 0) (const (+))

--bruna :: (Depth -> (Bruna, Int)) ...

-- bbeta :: Bruna -> Bruna -> Bruna
-- bbeta body arg = fst $ bruna 
--   (\d -> (BBin, 0)) 
--   (\n d -> case compare n (d + 1) of
--     EQ -> (inc d arg, stars arg)  --substitute
--     GT -> (BVar $ pred n, 0)      --decrement free variable
--     LT -> (BVar $ n, 0)           --do nothing
--   ) 
--   (\o f g d -> case o of
--     LImp -> (BOp LImp (fst (f d)) (fst (g (d + snd (f d)))), snd (f d) + snd (g (d + snd (f d))))
--     o -> (BOp o (f d) (g d), snd (f d) + snd (g d)) 
--   ) 
--   body 0

-- inc = const id

breduceNormal :: Bruna -> Maybe Bruna
breduceNormal b = undefined

normalizeBruna :: Bruna -> [Bruna]
normalizeBruna b = undefined

---------------------------------------------------------------
--Test Bruna Expressions

best :: String -> Bruna
best s = fromRight BBin $ parseBruna s

best1 = best "(* -> * -> 4 2 (* -> 1 3)) (* -> 5 1)"