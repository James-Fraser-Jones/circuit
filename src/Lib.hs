{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( fileAccess
    ) where

import qualified Data.Text as T

---------------------------------------------------------------

fileAccess :: IO ()
fileAccess = do
    input <- readFile "src/in.txt"
    writeFile "src/out.txt" input

---------------------------------------------------------------

data Lambda = Lam Id Lambda
            | App Lambda Lambda
            | Var Id

newtype Id = Id { unId :: T.Text }

instance Show Lambda where
    show l = case l of
        Lam id l -> "(" ++ "λ" ++ show id ++ "." ++ show l ++ ")"
        App l m -> "(" ++ show l ++ " " ++ show m ++ ")"
        Var id -> show id
    
instance Show Id where
    show id = show $ unId id

--parsec for this?
parse :: String -> Either () Lambda
parse = undefined

---------------------------------------------------------------

data Brujin = Bam Brujin
            | Bapp Brujin Brujin
            | Ind Int

instance Show Brujin where
    show b = case b of
        Bam b -> "(" ++ "λ" ++ "." ++ show b ++ ")"
        Bapp b c -> "(" ++ show b ++ " " ++ show c ++ ")"
        Ind n -> show n

brujin :: Lambda -> Brujin
brujin = undefined

reduce :: Brujin -> Brujin
reduce = undefined

---------------------------------------------------------------

data Circuit = Circuit

instance Show Circuit where
    show c = undefined

circuit :: Brujin -> Circuit
circuit = undefined

--Stuff about Text vs String
--https://hackage.haskell.org/package/text-show
--http://dev.stephendiehl.com/hask/#text.builder