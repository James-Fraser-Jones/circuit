module Lib
    ( fileAccess
    ) where

import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad

import GHC.IO.Encoding

---------------------------------------------------------------
--Example Constants

fixpoint_string :: String
fixpoint_string = "\\f.(\\x.f (x x)) (\\x.f (x x))"

example :: Circuit
example = Circuit ([[EMPTY, FULL, EMPTY],[FULL, EMPTY, FULL],[EMPTY, FULL, EMPTY]]) (3, 3) []

example2 :: Circuit
example2 = Circuit ([[FULL,FULL,FULL,FULL,FULL],[FULL,FULL,FULL,FULL,FULL]]) (5, 2) [(0,1), (2,1), (4,1)]

exampleIndices :: [(Int, Int)]
exampleIndices = [(0,3),(3,2),(4,0),(7,1),(8,4),(13,0),(16,9)]

---------------------------------------------------------------
--Generic Helpers

findLast :: (a -> Bool) -> [a] -> Maybe a
findLast p = foldl' (\b a -> if p a then Just a else b) Nothing

---------------------------------------------------------------
--Input/Output

fileAccess :: IO ()
fileAccess = do
    setLocaleEncoding utf8
    input <- readFile "src/in.txt"
    let output = either id (show . circuit . brujin) (lambda input)
    writeFile "src/out.txt" output

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
--De Brujin Expression Conversion Contexts

newtype Context = Context { unContext :: [(String, Int)] } deriving Show

emptyContext :: Context
emptyContext = Context []

updateContext :: String -> Context -> Context
updateContext s (Context c) = 
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
    Lam s l -> BLam $ brujin' (updateContext s c) l
    App a b -> BApp (brujin' c a) (brujin' c b)
    Var s -> BInd $ maybe 0 id $ lookup s (unContext c)

---------------------------------------------------------------
--De Brujin Expression Reduction

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

data Symbol = EMPTY --Basic symbols
            | FULL
            | QUEST

            | TLS --Generic box creation with wire threading
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

            | APP --Special symbols for drawing arrows and wires
            | LAM
            | TEE
            | PLUS
            | DOT
            | ARROW
            deriving (Enum)

symbols :: String
symbols = " █?" <> "┌┐└┘│─┼╔╗╚╝║═╪┏┓┗┛┃━┿" <> "┠╫┬┼●>"

symbolToChar :: Symbol -> Char
symbolToChar s = symbols !! fromEnum s

charToSymbol :: Char -> Maybe Symbol
charToSymbol c = toEnum <$> findIndex (== c) symbols

boxSymbols :: Bool -> (Symbol, Symbol, Symbol, Symbol, Symbol, Symbol, Symbol)
boxSymbols isBold =
    if isBold then
        (TLB, TRB, BLB, BRB, VEB, HOB, HOBS)
    else
        (TLD, TRD, BLD, BRD, VED, HOD, HODS)

---------------------------------------------------------------
--Circuit Manipulation Combinators

shiftIndices :: Int -> [(Int, Int)] -> [(Int, Int)]
shiftIndices n = fmap (\(pos, ind) -> (pos + n, ind))

updateSymbols :: Bool -> Int -> Int -> [Symbol] -> Circuit -> Circuit
updateSymbols isHorizontal x y sym (Circuit g s i) = 
    if isHorizontal then 
        let l = g !! y 
            l' = take x l <> sym <> drop (x + length sym) l
            g' = take y g <> pure l' <> drop (y + 1) g
         in Circuit g' s i
    else 
        let l = (!! x) <$> g  
            l' = take y l <> sym <> drop (y + length sym) l
            g' = zipWith (<>) (zipWith (<>) (take x <$> g) (pure <$> l')) ((drop $ x + 1) <$> g)
         in Circuit g' s i

pad :: Int -> Int -> Int -> Int -> Circuit -> Circuit
pad top bottom left right = padRight right . padBottom bottom . padLeft left . padTop top

padTop :: Int -> Circuit -> Circuit
padTop n (Circuit g (x, y) i) = Circuit g' (x, y + n) i
    where g' = replicate n t <> g
          t = (\n -> if n `elem` (fst <$> i) then VES else EMPTY) <$> [0..(x-1)]

padLeft :: Int -> Circuit -> Circuit
padLeft n (Circuit g (x, y) i) = Circuit g' (x + n, y) i'
    where g' = zipWith (<>) (replicate y $ replicate n EMPTY) g
          i' = shiftIndices n i

padRight :: Int -> Circuit -> Circuit
padRight n (Circuit g (x, y) i) = Circuit g' (x + n, y) i
    where g' = zipWith (<>) g (replicate y $ replicate n EMPTY)

padBottom :: Int -> Circuit -> Circuit
padBottom n (Circuit g (x, y) i) = Circuit g' (x, y + n) i
    where g' = g <> (replicate n $ replicate x EMPTY)

box :: Bool -> Circuit -> Circuit
box isBold (Circuit g (x, y) i) = Circuit g' (x + 2, y + 2) i'
    where i' = shiftIndices 1 i
          (tl, tr, bl, br, ve, ho, hos) = boxSymbols isBold
          g' = right $ left $ bottom $ top g
          top gri = pure t <> gri
          t = (\n -> if n `elem` (fst <$> i) then hos else ho) <$> [0..(x-1)]
          bottom gri = gri <> (pure $ replicate x ho)
          left gri = zipWith (<>) (pure (pure tl) <> replicate y (pure ve) <> pure (pure bl)) gri
          right gri = zipWith (<>) gri (pure (pure tr) <> replicate y (pure ve) <> pure (pure br)) 

append :: Int -> Circuit -> Circuit -> Circuit
append n (Circuit g (x, y) i) c2 = Circuit g'' (x'', y'') i''
    where (Circuit g' (x', y') i') = padLeft n c2
          g'' = zipWith (<>) g g'
          x'' = x + x'
          y'' = max y y'
          i'' = i ++ (shiftIndices x i') 

valign :: Circuit -> Circuit -> (Circuit, Circuit) --Vertical alignment goes low in case of odd/even mismatch
valign c1@(Circuit g (x, y) i) c2@(Circuit g' (x', y') i') = 
    let ydiff = y - y'
        c1' = pad ((abs ydiff + 1) `div` 2) (abs ydiff `div` 2) 0 0 c1
        c2' = pad ((abs ydiff + 1) `div` 2) (abs ydiff `div` 2) 0 0 c2
     in 
        if ydiff > 0 then
            (c1, c2')
        else if ydiff < 0 then
            (c1', c2)
        else 
            (c1, c2)

---------------------------------------------------------------
--Circuits

data Circuit = Circuit { grid :: [[Symbol]], size :: (Int, Int), indices :: [(Int, Int)] }

instance Show Circuit where
    show c = unlines $ fmap (fmap symbolToChar) $ grid c

emptyCircuit :: Circuit
emptyCircuit = Circuit [] (0, 0) []

circuit :: Brujin -> Circuit
circuit b = case b of
    BLam x -> arrow False $ box False $ wires $ pad 1 0 3 1 $ circuit x
    BApp x y -> uncurry (append 1) $ valign (arrow True $ box True $ pad 0 0 1 1 $ circuit y) $ circuit x
    BInd n -> 
        if n < 1 then
            Circuit [[QUEST, QUEST]] (2, 1) []
        else
            Circuit [[FULL, FULL]] (2, 1) [(0, n)]

arrow :: Bool -> Circuit -> Circuit --adds application and lambda arrows (arrows go high in case of even height)
arrow isApp c =
    if isApp then
        let c2@(Circuit _ (x, y) _) = pad 0 0 0 3 c
         in updateSymbols True (x - length appArrow) ((y - 1) `div` 2) appArrow c2
    else
        let c2@(Circuit _ (_, y) _) = pad 0 0 2 0 c
         in updateSymbols True 0 ((y - 1) `div` 2) lamArrow c2

appArrow :: [Symbol]
appArrow = [APP, HOS, HOS, ARROW]

lamArrow :: [Symbol]
lamArrow = [ARROW, HOS, LAM, HOS]

wires :: Circuit -> Circuit --adds wiring to padding before drawing a double-line box
wires c@(Circuit g (x, y) i) =
    let i' = fmap (fmap pred) i --decrement all indices
     in if isJust $ findLast (\(pos, ind) -> ind == 0) i' then
            undefined
        else 
            let Circuit g' _ _ = updateSymbols True 1 ((y - 1) `div` 2) [DOT] c
             in Circuit g' (x, y) i'