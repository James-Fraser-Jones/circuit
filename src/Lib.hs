module Lib
    ( reduceCircuit
    ) where

import Types
import Parser
import Lambda
import Brujin
import Circuit

import GHC.IO.Encoding
import Data.List
import Control.Monad
import Data.Traversable

reduceExpr :: Int -> IO ()
reduceExpr limit = do
    setLocaleEncoding utf8
    input <- readFile "src/in.txt"
    let lambda = parseLambda input
        reductions = (take limit . normalizeLambda) <$> lambda
     in writeFile "src/out.txt" $ either id (join . intersperse "\n" . fmap show) reductions

reduceCircuit :: Int -> IO ()
reduceCircuit limit = do
    setLocaleEncoding utf8
    input <- readFile "src/in.txt"
    let lambda = parseLambda input
        reductions = (take limit . normalizeLambda) <$> lambda
        brujins = reductions >>= traverse convertBrujin 
     in writeFile "src/out.txt" $ either id (join . intersperse "\n\n" . fmap (show . circuitBrujin)) brujins

parseIn :: IO ()
parseIn = do
    input <- readFile "src/in.txt"
    putStrLn $ either id show (parseLambda input)

---------------------------------------------------------------

--TODO:
--Fix up circuits so they still work with quotation
--Add comment ability
--Add simple "let" expressions (with foward dependencies)????
--Start working on mutual "letrec" expressions using list encoding and fixpoly

{-
example = "(λg. λf. λx. g (f x)) (λf. λx. λy. f y x) (λf. λx. λy. f y x)"                                --flip . flip = ($)
example2 = "(λx. λy. (λs. λz. x s (y s z))) (λs. λz. s (s z)) (λs. λz. s (s (s z)))"                     --2 + 3 = 5
example3 = "(λf. (λx. f (x x)) (λx. f (x x))) (λx. x)"                                                   --fixpoint combinator applied to id
example4 = "(λf. (λx. f (x x)) (λx. f (x x))) (λe. λm. m (λx.x) (λm. λn.(e m) (e n)) (λm. λv. e (m v)))" --self interpreting expression

nil = (\n c -> n)
cons = (\h t n c -> c h t)
head = (\l -> l (\n c -> n) (\h t -> h))
tail = (\l -> l (\n c -> n) (\h t -> t))
-}

fix :: (a -> a) -> a
fix f = f $ fix f

--http://okmij.org/ftp/Computation/fixed-point-combinators.html (black magic for poly-varidic mutual recursion)
fixpoly :: [[a] -> a] -> [a]
fixpoly l = fix (\self -> map ($ self) l) --self = fix (\self -> map ($ self) l)

--fixpoly :: [[Lambda] -> Lambda] -> [Lambda]

evenn :: Int -> Bool
evenn 0 = True
evenn n = odd $ pred n

oddd :: Int -> Bool
oddd 0 = False
oddd n = even $ pred n

evenn' :: [(Int -> Bool)] -> Int -> Bool
evenn' _ 0 = True
evenn' [e, o] n = o $ pred n

oddd' :: [(Int -> Bool)] -> Int -> Bool
oddd' _ 0 = False
oddd' [e, o] n = e $ pred n

evenodd :: [Int -> Bool]
evenodd = fixpoly [evenn', oddd']

evenn'' = evenodd !! 0
oddd'' = evenodd !! 1

{-
evenn'' 7
= (evenodd !! 0) 7
= ((fixpoly [evenn', oddd']) !! 0) 7 
= ((fix (\self -> map ($ self) [evenn', oddd'])) !! 0) 7

= ((fix (\self -> [evenn' self, oddd' self])) !! 0) 7
= (((\self -> [evenn' self, oddd' self]) (fix (\self -> [evenn' self, oddd' self]))) !! 0) 7
= (([evenn' (fix (\self -> [evenn' self, oddd' self])), oddd' (fix (\self -> [evenn' self, oddd' self]))]) !! 0) 7

= evenn' (fix (\self -> [evenn' self, oddd' self])) 7
= evenn' ((\self -> [evenn' self, oddd' self]) (fix (\self -> [evenn' self, oddd' self]))) 7
= evenn' [evenn' (fix (\self -> [evenn' self, oddd' self])), oddd' (fix (\self -> [evenn' self, oddd' self]))] 7
= oddd' (fix (\self -> [evenn' self, oddd' self])) (pred 7)

= oddd' (fix (\self -> [evenn' self, oddd' self])) 6
= ...
-}