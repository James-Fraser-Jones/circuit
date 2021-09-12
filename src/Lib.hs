module Lib
    ( reduceCircuit
    ) where

import Data.Maybe
import Data.Either
import Data.List
import Control.Applicative
import Control.Monad

import Types
import Parser
import Lambda
import Brujin
import Circuit

import GHC.IO.Encoding
import Data.Traversable

{-
example = "(λg. λf. λx. g (f x)) (λf. λx. λy. f y x) (λf. λx. λy. f y x)"                                --flip . flip = ($)
example2 = "(λx. λy. (λs. λz. x s (y s z))) (λs. λz. s (s z)) (λs. λz. s (s (s z)))"                     --2 + 3 = 5
example3 = "(λf. (λx. f (x x)) (λx. f (x x))) (λx. x)"                                                   --fixpoint combinator applied to id
example4 = "(λf. (λx. f (x x)) (λx. f (x x))) (λe. λm. m (λx.x) (λm. λn.(e m) (e n)) (λm. λv. e (m v)))" --self interpreting expression
-}

reduceCircuit :: Int -> IO ()
reduceCircuit limit = do
    setLocaleEncoding utf8
    input <- readFile "src/in.txt"
    let lambda = parseLambda input
        reductions = (take limit . normalizeLambda) <$> lambda
        brujins = reductions >>= traverse convertBrujin 
     in do
         putStrLn $ either id (join . intersperse "\n" . fmap show) reductions
         writeFile "src/out.txt" $ either id (join . intersperse "\n\n" . fmap (show . circuitBrujin)) brujins
