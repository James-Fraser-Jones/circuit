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
        Lam s l -> "\\" <> s <> "." <> show l
        App a b -> (if isLam a then bracket else id) (show a) <> " " <> (if isLam b || isApp b then bracket else id) (show b)
        Var s -> s

--megaparsec for this?
parse :: String -> Either () Lambda
parse = undefined

bracket :: String -> String
bracket s = "(" <> s <> ")"

isLam :: Lambda -> Bool
isLam l = case l of
    Lam _ _ -> True
    _ -> False

isApp :: Lambda -> Bool
isApp l = case l of
    App _ _ -> True
    _ -> False

fixpoint :: Lambda
fixpoint = 
    Lam (
        "f"
    )(
        App (
            Lam (
                "x"
            )(
                App (
                    Var "f"
                )(
                    App (
                        Var "x"
                    )(
                        Var "x"
                    )
                )
            )
        )(
            Lam (
                "x"
            )(
                App (
                    Var "f"
                )(
                    App (
                        Var "x"
                    )(
                        Var "x"
                    )
                )
            )
        )
    )

---------------------------------------------------------------

newtype Context = Context { unContext :: [(String, Int)] } deriving Show

emptyContext :: Context
emptyContext = Context []

update :: String -> Context -> Context
update s (Context c) = 
    let c' = fmap (fmap succ) c 
     in Context $ case findIndex (\(s', _) -> s' == s) c of
            Just n -> take n c' <> [(s, 1)] <> drop (n + 1) c'
            Nothing -> (s, 1) : c'

---------------------------------------------------------------

data Brujin = BLam Brujin
            | BApp Brujin Brujin
            | BInd Int

isBLam :: Brujin -> Bool
isBLam b = case b of
    BLam _ -> True
    _ -> False

isBApp :: Brujin -> Bool
isBApp b = case b of
    BApp _ _ -> True
    _ -> False

instance Show Brujin where
    show b = case b of
        BLam b -> "\\ " <> show b
        BApp b c -> (if isBLam b then bracket else id) (show b) <> " " <> (if isBLam c || isBApp c then bracket else id) (show c)
        BInd n -> show n

brujin :: Lambda -> Brujin
brujin = brujin' emptyContext

brujin' :: Context -> Lambda -> Brujin
brujin' c l = case l of
    Lam s l -> BLam $ brujin' (update s c) l
    App a b -> BApp (brujin' c a) (brujin' c b)
    Var s -> BInd $ maybe (-1) id $ lookup s (unContext c)

substitute :: Brujin -> Brujin -> Brujin
substitute sub b = substitute' 0 sub b

substitute' :: Int -> Brujin -> Brujin -> Brujin
substitute' n sub b = case b of
    BLam b' -> BLam $ substitute' (succ n) sub b'
    BApp b1 b2 -> BApp (substitute' n sub b1) (substitute' n sub b2)
    BInd n' -> if n' == n then sub else BInd n'

decrement :: Brujin -> Brujin
decrement b = case b of
    BLam b' -> BLam $ decrement b'
    BApp b1 b2 -> BApp (decrement b1) (decrement b2)
    BInd n -> BInd $ pred n

reduce_normal :: Brujin -> Maybe Brujin
reduce_normal b = case b of
    BApp (BLam b1) b2 -> Just $ substitute b2 $ decrement b1    --reduce outer before inner
    BApp b1 b2 -> case reduce_normal b1 of                      --reduce left values before right ones
        Just b' -> Just $ BApp b' b2
        Nothing -> BApp b1 <$> reduce_normal b2
    BLam b' -> BLam <$> reduce_normal b'
    BInd n -> Nothing

normalize :: Brujin -> [Brujin]
normalize b = b : maybe [] normalize (reduce_normal b)

--putStr $ unlines $ fmap show $ take 5 $ normalize $ brujin fixpoint

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