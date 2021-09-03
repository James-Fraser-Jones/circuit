{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( fileAccess
    ) where

import Data.List
import GHC.IO.Encoding

---------------------------------------------------------------

fileAccess :: IO ()
fileAccess = do
    setLocaleEncoding utf8
    input <- readFile "src/in.txt"
    writeFile "src/out.txt" input

---------------------------------------------------------------

data Lambda = Lam String Lambda
            | App Lambda Lambda
            | Var String

instance Show Lambda where
    show l = case l of
        Lam s l -> "(λ" <> s <> "." <> show l <> ")"
        App a b -> "(" <> show a <> " " <> show b <> ")"
        Var s -> s

--parsec for this?
parse :: String -> Either () Lambda
parse = undefined

---------------------------------------------------------------

newtype Context = Context { unContext :: [(String, Int)] }

data Brujin = BLam Brujin
            | BApp Brujin Brujin
            | BInd Int

instance Show Brujin where
    show b = case b of
        BLam b -> "(λ." <> show b <> ")"
        BApp b c -> "(" <> show b <> " " <> show c <> ")"
        BInd n -> show n

brujin :: Context -> Lambda -> Brujin
brujin c l = case l of
    Lam s l -> BLam $ brujin (update s c) l
    App a b -> BApp (brujin c a) (brujin c b)
    Var s -> BInd $ maybe (-1) id $ lookup s (unContext c)

update :: String -> Context -> Context
update s c = Context $ (s, 0) : fmap (fmap succ) (unContext c) 

reduce :: Brujin -> Brujin
reduce = undefined

---------------------------------------------------------------

data Symbol = EMPTY
            | FULL
            | QUEST
            | TLS
            | TRS
            | BLS
            | BRS
            | VES
            | HOS
            | HOSS
            | TLD
            | TRD
            | BLD
            | BRD
            | VED
            | HOD
            | HODS
            | TLB
            | TRB
            | BLB
            | BRB
            | VEB
            | HOB
            | HOBS
            deriving (Enum)

symbols :: String
symbols = " █?┌┐└┘│─┼╔╗╚╝║═╪┏┓┗┛┃━┿"

symbolToChar :: Symbol -> Char
symbolToChar s = symbols !! fromEnum s

charToSymbol :: Char -> Maybe Symbol
charToSymbol c = toEnum <$> findIndex (== c) symbols

---------------------------------------------------------------

data Circuit = Circuit { grid :: [[Symbol]], size :: (Int, Int), indices :: [(Int, Int)] }

instance Show Circuit where
    show c = unlines $ fmap (fmap symbolToChar) $ grid c

emptyCircuit :: Circuit
emptyCircuit = Circuit [] (0, 0) []

circuit :: Brujin -> Circuit
circuit b = case b of
    BLam b -> undefined
    BApp b c -> undefined
    BInd n -> 
        if n < 0 then
            Circuit [[QUEST, QUEST]] (2, 1) []
        else
            Circuit [[FULL, FULL]] (2, 1) [(0, n)]

---------------------------------------------------------------

-- data BoxStyle = EmptyStyle
--               | SingleStyle
--               | DoubleStyle
--               | BoldStyle

-- newtype StyleChars = StyleChars (Char, Char, Char, Char, Char, Char)

-- empty :: StyleChars
-- empty = StyleChars (' ', ' ', ' ', ' ', ' ', ' ')

-- single :: StyleChars
-- single = StyleChars ('┌', '┐', '└', '┘', '─', '│')

-- double :: StyleChars
-- double = StyleChars ('╔', '╗', '╚', '╝', '═', '║')

-- bold :: StyleChars
-- bold = StyleChars ('┏', '┓', '┗', '┛', '━', '┃')

-- getStyleChars :: BoxStyle -> StyleChars
-- getStyleChars b = case b of
--     EmptyStyle -> empty
--     SingleStyle -> single
--     DoubleStyle -> double
--     BoldStyle -> bold

-- fixpoint :: Circuit 
-- fixpoint = 
--     Box DoubleStyle (
--         Adjacent ( 
--             Box BoldStyle (
--                 Box DoubleStyle (
--                     Adjacent (
--                         Box BoldStyle (
--                             Adjacent (
--                                 Box BoldStyle (
--                                     Variable True
--                                 )
--                             )(
--                                 Variable True
--                             )
--                         )
--                     )(
--                         Variable True
--                     )
--                 )
--             )
--         )(
--             Box DoubleStyle (
--                 Adjacent (
--                     Box BoldStyle (
--                         Adjacent (
--                             Box BoldStyle (
--                                 Variable True
--                             )
--                         )(
--                             Variable True
--                         )
--                     )
--                 )(
--                     Variable True
--                 )
--             )
--         )
--     )

-- data Circuit = Box BoxStyle Circuit
--              | Adjacent Circuit Circuit
--              | Variable Bool

-- instance Show Circuit where
--     show c = case c of
--         Box s c -> 
--             let StyleChars (tl, tr, bl, br, h, v) = getStyleChars s
--                 lin = lines $ show c
--                 len = length $ head lin
--                 top_line = pure tl ++ replicate len h ++ pure tr
--                 bottom_line = pure bl ++ replicate len h ++ pure br
--                 added_sides = map (\s -> pure v ++ s ++ pure v) lin
--              in unlines $ pure top_line ++ added_sides ++ pure bottom_line
--         Adjacent c1 c2 -> 
--             let l1 = lines $ show c1 
--                 l2 = lines $ show c2
--                 p = length l1 - length l2 --p = padding needed on l2 (negative means padding needed on l1)
--              in case compare p 0 of
--                     EQ -> unlines $ zipWith (++) l1 l2
--                     GT -> unlines $ zipWith (++) l1 (pad p l2)
--                     LT -> unlines $ zipWith (++) (pad (abs p) l1) l2 --pad l1
--         Variable bound -> if bound then "██" else "??"

-- pad :: Int -> [String] -> [String] --pad bottom first
-- pad n lines = 
--     let padder = replicate (length $ head lines) ' '
--      in replicate (n `div` 2) padder ++ lines ++ replicate ((n `div` 2) + (n `mod` 2)) padder

-- circuit :: Brujin -> Circuit
-- circuit = undefined

--Stuff about Text vs String
--https://hackage.haskell.org/package/text-show
--http://dev.stephendiehl.com/hask/#text.builder