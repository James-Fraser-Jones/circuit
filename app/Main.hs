module Main where

import Lib

import Options.Applicative
import GHC.IO.Encoding

data Args = Args { 
    expr :: String
  , lambda :: Bool
  , reductions :: Int
  , newlines :: Int
}

args :: Parser Args
args = Args
    <$> strOption
        ( long "expr"
       <> short 'e'
       <> metavar "STRING"
       <> help "Lambda expression to input")
    <*> switch
        ( long "lambda"
       <> short 'l'
       <> help "Whether to output Lambda expressions, as opposed to circuit diagrams"
        )
    <*> option auto
        ( long "reductions"
       <> short 'r'
       <> metavar "INT"
       <> help "Maximum number of reductions to compute before ending the program"
       <> showDefault
       <> value 100
        )
    <*> option auto
        ( long "newlines"
       <> short 'n'
       <> metavar "INT"
       <> help "Number of newlines to seperate reductions by"
       <> showDefault
       <> value 1
        )

--example: .stack-work/dist/x86_64-linux-tinfo6-libc6-pre232/Cabal-3.2.1.0/build/circuit-exe/circuit-exe -e "(\x -> x) (\y -> y)"
main :: IO ()
main = do
   setLocaleEncoding utf8
   parseArgs

parseArgs :: IO ()
parseArgs = execute =<< execParser opts
  where
    opts = info (args <**> helper)
      ( fullDesc
     <> progDesc "Reduce Lambda expression to normal form, generating circuit diagrams for each reduction step"
     <> header "circuit - a novel visualizer for the lambda calculus" )

execute :: Args -> IO ()
execute (Args e l r n) = (if l then runLambda else runCircuit) e r n
