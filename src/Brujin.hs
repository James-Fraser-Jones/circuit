module Brujin(brujin, convertBrujin, normalizeBrujin) where

import Types
import Utils
import Lambda

import Data.List

brujin :: Int -> Int -> String -> String
brujin i n s = either id (format i n normalizeBrujin) (parseLambda s >>= convertBrujin)

-- both :: Int -> Int -> String -> String
-- both i n s = case parseLambda s of
--     Left err -> err
--     Right l -> case convertBrujin l of
--         Left err -> err 
--         Right b -> 
--             let results = take i $ zip (normalizeLambda l) (normalizeBrujin b)
--                 strings = map f results
--                 f (l, b) = case convertBrujin l of
--                     Left err -> err
--                     Right b' -> show l <> "\n" <> show b' <> "\n" <> show b
--              in concat $ intersperse (replicate n '\n') $ strings

---------------------------------------------------------------
--Converting Lambda Expressions

emptyContext :: BrujinContext
emptyContext = BrujinContext []

updateContext :: String -> BrujinContext -> BrujinContext
updateContext s (BrujinContext c) = 
    let c' = fmap (fmap succ) c                                 --increment all indices currently in the context
     in BrujinContext $ case findIndex (\(s', _) -> s' == s) c of
            Just n -> take n c' <> [(s, 0)] <> drop (n + 1) c'  --if string was found, reset corresponding index to 0
            Nothing -> (s, 0) : c'                              --if not, add it to the context

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
        BCon s -> s
        BQte -> "#Quote"

convertBrujin' :: BrujinContext -> Lambda -> Either String Brujin
convertBrujin' (BrujinContext c) l = case l of
    Lam s l -> BLam <$> convertBrujin' (updateContext s $ BrujinContext c) l
    App a b -> BApp <$> (convertBrujin' (BrujinContext c) a) <*> (convertBrujin' (BrujinContext c) b)
    Var s -> maybe (Left $ "Scope Error: Variable \"" <> s <> "\" not in scope") (Right . BInd) (lookup s c)
    Mvar s -> Right $ BCon s
    Rei -> Right BQte

convertBrujin :: Lambda -> Either String Brujin
convertBrujin = convertBrujin' emptyContext

---------------------------------------------------------------
--Reducing Brujin Expressions

modifyIndices :: (Int -> Int -> Brujin) -> Brujin -> Brujin
modifyIndices = modifyIndices' 0

modifyIndices' :: Int -> (Int -> Int -> Brujin) -> Brujin -> Brujin
modifyIndices' d f b = case b of
    BLam b' -> BLam $ modifyIndices' (d + 1) f b'
    BApp b1 b2 -> BApp (modifyIndices' d f b1) (modifyIndices' d f b2)
    BInd n -> f d n
    BCon s -> BCon s
    BQte -> BQte

--mark substitution sites with -1 and decrement free variables of function body
prepare :: Int -> Int -> Brujin
prepare depth index = BInd $
    if index == depth then
        -1
    else if isFree depth index then
        index - 1
    else
        index

--increment free variables by n
increment :: Int -> Int -> Int -> Brujin
increment n depth index = BInd $
    if isFree depth index then
        index + n
    else
        index

substitute :: Brujin -> Int -> Int -> Brujin
substitute arg depth index = 
    if index == -1 then
        modifyIndices (increment depth) arg 
    else 
        BInd index

beta :: Brujin -> Brujin -> Brujin
beta body arg = modifyIndices (substitute arg) (modifyIndices prepare body)

isFree :: Int -> Int -> Bool
isFree depth index = index >= depth 

reduceNormal :: Brujin -> Maybe Brujin
reduceNormal b = case b of
    BApp BQte t -> Just $ quote t
    BApp (BLam b1) b2 -> Just $ beta b1 b2       --reduce outer before inner
    BApp b1 b2 -> case reduceNormal b1 of        --reduce left values before right ones
        Just b' -> Just $ BApp b' b2
        Nothing -> BApp b1 <$> reduceNormal b2
    BLam b' -> BLam <$> reduceNormal b'
    BInd n -> Nothing
    BCon s -> Nothing

normalizeBrujin :: Brujin -> [Brujin]
normalizeBrujin b = b : maybe [] normalizeBrujin (reduceNormal b)

---------------------------------------------------------------
--Quotation and Interpretation

quote :: Brujin -> Brujin
quote b = let inc = modifyIndices (increment 5) in
    foldr (.) id (replicate 5 BLam) $ case b of
        BInd n -> BApp (BInd 4) (inc $ BInd n)
        BApp t u -> BApp (BApp (BInd 3) (quote (inc t))) (quote (inc u))
        BLam t -> let BLam t' = inc $ BLam t
                   in BApp (BInd 2) (BLam $ quote t')
        BCon s -> BApp (BInd 1) (BCon s)
        BQte -> BInd 0