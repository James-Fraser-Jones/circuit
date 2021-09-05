module Lib
    ( fileAccess
    ) where

import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad

import GHC.IO.Encoding

---------------------------------------------------------------

fileAccess :: IO ()
fileAccess = do
    setLocaleEncoding utf8
    input <- readFile "src/in.txt"
    writeFile "src/out.txt" input

---------------------------------------------------------------

fixpoint_string :: String
fixpoint_string = "\\f.(\\x.f (x x)) (\\x.f (x x))"

 --whitespace with specifically ')' '(' or some lowercase letters either side 
data Token = White
           | Lambda
           | Dot
           | Open
           | Close
           | Str String
           deriving (Eq, Show)

--this is all kinda ugly, there's got to be a slightly nicer way :(
token :: String -> [Token]
token s = reverse $ token' s []

token' :: String -> [Token] -> [Token]
token' "" ts = ts
token' (c:cs) [] = token' cs $ pure $ getToken c
token' (c:cs) (t:ts) = token' cs $ case getToken c of
    White -> if t == White then t:ts else White:t:ts
    Str s -> case t of
        Str s' -> (Str $ s' <> s):ts
        t -> (Str s):t:ts
    tok -> tok:t:ts

getToken :: Char -> Token
getToken c = case c of
    '(' -> Open
    ')' -> Close 
    '\\' -> Lambda
    '.' -> Dot
    ' ' -> White
    x -> Str $ pure x

findApps :: String -> String
findApps (a:b:c:xs) = 
    if b == ' ' then
        if ((a == ')' || a `elem` ['a'..'z']) && ((c == '(' || c `elem` ['a'..'z']) then
            a:'@':(findApps (c:xs))
        else
            findApps (a:c:xs)
    else
        a:(findApps (b:c:xs))
---------------------------------------------------------------

data Lambda = Lam String Lambda
            | App Lambda Lambda
            | Var String

instance Show Lambda where
    show l = case l of
        Lam s l -> "\\" <> s <> "." <> show l
        App a b -> (if isLam a then bracket else id) (show a) <> " " <> (if isLam b || isApp b then bracket else id) (show b)
        Var s -> s

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

lambda :: [Token] -> Lambda
lambda = undefined --TODO

fixpoint_lambda :: Lambda
fixpoint_lambda = 
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

(!?) :: [a] -> Int -> Maybe a
list !? n = if length list - 1 < n then Nothing else Just $ list !! n

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
    BLam b -> undefined --TODO
    BApp b c -> undefined --TODO
    BInd n -> 
        if n < 0 then
            Circuit [[QUEST, QUEST]] (2, 1) []
        else
            Circuit [[FULL, FULL]] (2, 1) [(0, n)]