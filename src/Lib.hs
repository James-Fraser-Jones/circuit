module Lib
    ( fileAccess
    ) where

import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad

import GHC.IO.Encoding

---------------------------------------------------------------
--Constants

fixpoint_string :: String
fixpoint_string = "\\f.(\\x.f (x x)) (\\x.f (x x))"

---------------------------------------------------------------
--Input/Output

fileAccess :: IO ()
fileAccess = do
    setLocaleEncoding utf8
    input <- readFile "src/in.txt"
    writeFile "src/out.txt" input

---------------------------------------------------------------
--Parsing Types and Instances

newtype Parser a = Parser { parse :: String -> Either String (String, a) }

instance Functor Parser where
    fmap = (=<<) . (.) return

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Monad Parser where
    return a = Parser $ \s -> Right (s, a)
    pa >>= f = Parser $ \s -> case parse pa s of
        Right (s', a) -> parse (f a) s'
        Left err -> Left err

instance Alternative Parser where
    empty = Parser $ \s -> Left "Parse Error: Failure Parser"
    pa <|> pb = Parser $ \s -> case parse pa s of
        Left err -> parse pb s
        val -> val

---------------------------------------------------------------
--Parsing Combinators

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \s -> case s of
    [] -> Left "Parse Error: Empty Input"
    (x:xs) -> 
        if predicate x then
            Right (xs, x)
        else
            Left $ "Parse Error: Predicate Failed On Character: " <> pure x

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

iden :: Parser String
iden = some $ oneOf ['a'..'z']

char :: Char -> Parser Char
char c = satisfy (c ==)

spaces :: Parser String
spaces = many $ oneOf " \t\n\r"

stripWhitespace :: Parser a -> Parser a
stripWhitespace pa = do
    spaces
    a <- pa
    spaces
    return a

parens :: Parser a -> Parser a
parens pa = do
    char '('
    spaces
    a <- pa
    spaces
    char ')'
    return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a --magic to avoid problems with left recursion
pa `chainl1` op = do 
    a <- pa
    rest a
    where 
        rest x = ( do 
            f <- op
            y <- pa
            rest (f x y)
            ) <|> return x

finish :: String -> Parser a -> Either String a
finish s pa = case parse pa s of
    Right ("", a) -> Right a
    Right (s, a) -> Left $ "Parse Error: Input Not Fully Consumed. Remaining: " <> s
    Left err -> Left err

---------------------------------------------------------------
--Parsing Lambda Expressions

expr :: Parser Lambda
expr = term `chainl1` app

app :: Parser (Lambda -> Lambda -> Lambda)
app = do
    spaces
    return App

term :: Parser Lambda
term = lam <|> var <|> parens expr

lam :: Parser Lambda
lam = do
    oneOf ['\\', 'λ']
    spaces
    s <- iden
    spaces
    char '.'
    spaces
    e <- expr
    return $ Lam s e

var :: Parser Lambda
var = do
    s <- iden
    return $ Var s

---------------------------------------------------------------
--Lambda Expressions

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

lambda :: String -> Either String Lambda
lambda s = finish s $ stripWhitespace expr

---------------------------------------------------------------
--Expression Reduction Contexts

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
--De Brujin Expressions

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

---------------------------------------------------------------
--Circuit Symbols

data Symbol = EMPTY
            | FULL
            | QUEST

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

            | LAM
            | APP
            | TEE
            deriving (Enum)

symbols :: String
symbols = " █?" <> "╔╗╚╝║═╪┏┓┗┛┃━┿" <> "╫┠┬"

symbolToChar :: Symbol -> Char
symbolToChar s = symbols !! fromEnum s

charToSymbol :: Char -> Maybe Symbol
charToSymbol c = toEnum <$> findIndex (== c) symbols

boxSymbols :: Bool -> (Symbol, Symbol, Symbol, Symbol, Symbol, Symbol, Symbol)
boxSymbols isBold =
    if isBold then
        (TLD, TRD, BLD, BRD, VED, HOD, HODS)
    else
        (TLB, TRB, BLB, BRB, VEB, HOB, HOBS)

---------------------------------------------------------------
--Circuits

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

intHalfer :: Int -> (Int, Int) --returns (smaller, larger)
intHalfer n = (n `div` 2, n `div` 2 + n `mod` 2)

shiftIndices :: Int -> Circuit -> Circuit
shiftIndices n c = undefined

decrementIndices :: Circuit -> Circuit
decrementIndices c = undefined

validIndices :: Circuit -> [Int] --returns locations of all indices with a 1
validIndices c = undefined

pad :: Int -> Int -> Int -> Int -> Circuit -> Circuit
pad top bottom left right c = undefined

box :: Bool -> Circuit -> Circuit
box isBold c = undefined

append :: Int -> Circuit -> Circuit -> Circuit
append n c1 c2 = undefined

align :: Circuit -> Circuit -> (Circuit, Circuit)
align c1 c2 = undefined

apper :: Circuit -> Circuit --adds application arrow
apper c = undefined

lammer :: Circuit -> Circuit --adds abstraction arrow with wiring
lammer c = undefined