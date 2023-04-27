module Lib where

import Types
import Parser
import Lambda
import Brujin
import Circuit

import GHC.IO.Encoding

--e.g. runFile "example" "lambda" $ lambda 100 1
--e.g. runFile "example" "circuit" $ circuit 100 1
runFile :: String -> String -> (String -> String) -> IO () 
runFile file appended f = do
    setLocaleEncoding utf8
    input <- readFile ("io/" ++ file ++ ".txt")
    writeFile ("io/" ++ file ++ "_" ++ appended ++ "_out.txt") $ f input

runPrint :: String -> (String -> String) -> IO () --e.g. runPrint "example" $ lambdaSingle 20
runPrint file f = do
    setLocaleEncoding utf8
    input <- readFile ("io/" ++ file ++ ".txt")
    putStrLn $ f input