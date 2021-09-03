{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( fileAccess
    ) where

import qualified Data.Text as T
import GHC.IO.Encoding

---------------------------------------------------------------

showText :: Show a => a -> T.Text
showText = T.pack . show

---------------------------------------------------------------

fileAccess :: IO ()
fileAccess = do
    setLocaleEncoding utf8
    --input <- readFile "src/in.txt"
    writeFile "src/out.txt" (show fixpoint)

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

data Brujin = BLam Brujin
            | BApp Brujin Brujin
            | BInd Int

instance Show Brujin where
    show b = case b of
        BLam b -> "(λ." ++ show b ++ ")"
        BApp b c -> "(" ++ show b ++ " " ++ show c ++ ")"
        BInd n -> show n

brujin :: Lambda -> Brujin
brujin = undefined

reduce :: Brujin -> Brujin
reduce = undefined

---------------------------------------------------------------

data BoxStyle = EmptyStyle
              | SingleStyle
              | DoubleStyle
              | BoldStyle

newtype StyleChars = StyleChars (Char, Char, Char, Char, Char, Char)

empty :: StyleChars
empty = StyleChars (' ', ' ', ' ', ' ', ' ', ' ')

single :: StyleChars
single = StyleChars ('┌', '┐', '└', '┘', '─', '│')

double :: StyleChars
double = StyleChars ('╔', '╗', '╚', '╝', '═', '║')

bold :: StyleChars
bold = StyleChars ('┏', '┓', '┗', '┛', '━', '┃')

getStyleChars :: BoxStyle -> StyleChars
getStyleChars b = case b of
    EmptyStyle -> empty
    SingleStyle -> single
    DoubleStyle -> double
    BoldStyle -> bold

---------------------------------------------------------------

fixpoint :: Circuit 
fixpoint = 
    Box DoubleStyle (
        Adjacent ( 
            Box BoldStyle (
                Box DoubleStyle (
                    Adjacent (
                        Box BoldStyle (
                            Adjacent (
                                Box BoldStyle (
                                    Variable True
                                )
                            )(
                                Variable True
                            )
                        )
                    )(
                        Variable True
                    )
                )
            )
        )(
            Box DoubleStyle (
                Adjacent (
                    Box BoldStyle (
                        Adjacent (
                            Box BoldStyle (
                                Variable True
                            )
                        )(
                            Variable True
                        )
                    )
                )(
                    Variable True
                )
            )
        )
    )

data Circuit = Box BoxStyle Circuit
             | Adjacent Circuit Circuit
             | Variable Bool

instance Show Circuit where
    show c = case c of
        Box s c -> 
            let StyleChars (tl, tr, bl, br, h, v) = getStyleChars s
                lin = lines $ show c
                len = length $ head lin
                top_line = pure tl ++ replicate len h ++ pure tr
                bottom_line = pure bl ++ replicate len h ++ pure br
                added_sides = map (\s -> pure v ++ s ++ pure v) lin
             in unlines $ pure top_line ++ added_sides ++ pure bottom_line
        Adjacent c1 c2 -> 
            let l1 = lines $ show c1 
                l2 = lines $ show c2
                p = length l1 - length l2 --p = padding needed on l2 (negative means padding needed on l1)
             in case compare p 0 of
                    EQ -> unlines $ zipWith (++) l1 l2
                    GT -> unlines $ zipWith (++) l1 (pad p l2)
                    LT -> unlines $ zipWith (++) (pad (abs p) l1) l2 --pad l1
        Variable bound -> if bound then "██" else "??"

pad :: Int -> [String] -> [String] --pad bottom first
pad n lines = 
    let padder = replicate (length $ head lines) ' '
     in replicate (n `div` 2) padder ++ lines ++ replicate ((n `div` 2) + (n `mod` 2)) padder

circuit :: Brujin -> Circuit
circuit = undefined

--Stuff about Text vs String
--https://hackage.haskell.org/package/text-show
--http://dev.stephendiehl.com/hask/#text.builder