module Lib where

import Types
import Parser
import Lambda
import Brujin
import Circuit

--e.g. runFile "example" "lambda" $ lambda 100 1
--e.g. runFile "example" "circuit" $ circuit 100 1
runFileIO :: String -> String -> (String -> String) -> IO () 
runFileIO file appended f = do
    input <- readFile ("io/" ++ file ++ ".txt")
    writeFile ("io/" ++ file ++ "_" ++ appended ++ "_out.txt") $ f input

runFileI :: String -> (String -> String) -> IO () --e.g. runPrint "example" $ lambdaSingle 20
runFileI file f = do
    input <- readFile ("io/" ++ file ++ ".txt")
    putStrLn $ f input

runLambda :: String -> Int -> Int -> IO () --e.g. runLambda "(\x -> x) (\y -> y)" 100
runLambda expr reductions newlines = putStrLn $ lambda reductions newlines expr

runCircuit :: String -> Int -> Int -> IO () --e.g. runCircuit "(\x -> x) (\y -> y)" 100
runCircuit expr reductions newlines = putStrLn $ circuit reductions newlines expr