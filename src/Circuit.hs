module Circuit(circuit) where

import Types
import Lambda
import Brujin
import Utils

import Data.List

circuit :: Int -> Int -> String -> String
circuit max_iterations newline_spacing s = either id (format max_iterations newline_spacing $ fmap circuitBrujin . normalizeBrujin) (parseLambda s >>= convertBrujin)

---------------------------------------------------------------
--Circuit Symbols

space = ' '
full = '█'
appa = '┠'
lama = '╫'
tee = '┬'
dot = '●'
arr = '>'

tls = '┌'
trs = '┐'
bls = '└'
brs = '┘'
ves = '│'
hos = '─'
hoss = '┼'

tld = '╔'
trd = '╗'
bld = '╚'
brd = '╝'
ved = '║'
hod = '═'
hods = '╪'

tlb = '┏'
trb = '┓'
blb = '┗'
brb = '┛'
veb = '┃'
hob = '━'
hobs = '┿'

boxSymbols :: Bool -> (Symbol, Symbol, Symbol, Symbol, Symbol, Symbol, Symbol)
boxSymbols isBold =
    if isBold then
        (tlb, trb, blb, brb, veb, hob, hobs)
    else
        (tld, trd, bld, brd, ved, hod, hods)

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
          t = (\n -> if n `elem` (fst <$> i) then ves else space) <$> [0..(x-1)]

padLeft :: Int -> Circuit -> Circuit
padLeft n (Circuit g (x, y) i) = Circuit g' (x + n, y) i'
    where g' = zipWith (<>) (replicate y $ replicate n space) g
          i' = shiftIndices n i

padRight :: Int -> Circuit -> Circuit
padRight n (Circuit g (x, y) i) = Circuit g' (x + n, y) i
    where g' = zipWith (<>) g (replicate y $ replicate n space)

padBottom :: Int -> Circuit -> Circuit
padBottom n (Circuit g (x, y) i) = Circuit g' (x, y + n) i
    where g' = g <> (replicate n $ replicate x space)

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

--Vertical alignment goes high in case of odd/even mismatch
--If first circuit is odd height and second circuit is even height and second circuit is shorter then both boxes must be padded 1 extra on top
--This must happen before the app arrow is drawn on the first circuit
valign :: Circuit -> Circuit -> (Circuit, Circuit) 
valign c1@(Circuit g (x, y) i) c2@(Circuit g' (x', y') i') = 
    let ydiff = y - y'
        c1' = pad (abs ydiff `div` 2) ((abs ydiff + 1) `div` 2) 0 0 c1
        c2' = pad (abs ydiff `div` 2) ((abs ydiff + 1) `div` 2) 0 0 c2
     in 
        if ydiff > 0 then
            if even y' && odd y then
                (pad 1 0 0 0 c1, pad 1 0 0 0 c2')
            else
                (c1, c2')
        else if ydiff < 0 then
            (c1', c2)    
        else 
            (c1, c2)

---------------------------------------------------------------
--Circuits

instance Show Circuit where
    show = unlines . grid

emptyCircuit :: Circuit
emptyCircuit = Circuit [] (0, 0) []

arrow :: Bool -> Circuit -> Circuit --adds application and lambda arrows (arrows go high in case of even height)
arrow isApp c =
    if isApp then
        let c2@(Circuit _ (x, y) _) = pad 0 0 0 3 c
         in updateSymbols True (x - length appArrow) ((y - 1) `div` 2) appArrow c2
    else
        let c2@(Circuit _ (_, y) _) = pad 0 0 2 0 c
         in updateSymbols True 0 ((y - 1) `div` 2) lamArrow c2

appArrow :: [Symbol]
appArrow = [appa, hos, hos, arr]

lamArrow :: [Symbol]
lamArrow = [arr, hos, lama, hos]

wires :: Circuit -> Circuit --adds wiring to padding before drawing a double-line box
wires c@(Circuit g (x, y) i) =
    let i' = fmap (fmap pred) i --decrement all indices
     in case findLast (\(pos, ind) -> ind == -1) i' of
        Just (lastPos, _) ->
            let c2 = updateSymbols False 1 0 (arm y) c
                Circuit g' _ _ = updateSymbols True 2 0 (straight 2 lastPos i') c2
                i'' = filter (\(pos, ind) -> ind >= 0) i'
             in Circuit g' (x, y) i''
        Nothing ->
            let Circuit g' _ _ = updateSymbols True 1 ((y - 1) `div` 2) (pure dot) c
             in Circuit g' (x, y) i'
            
armLength :: Int -> Int
armLength n = ((n - 1) `div` 2) - 1

arm :: Int -> [Symbol]
arm n = if armLength n == -1 then pure hos else pure tls <> replicate (armLength n) ves <> pure brs

straight :: Int -> Int -> [(Int, Int)] -> [Symbol]
straight start last indices =
    let handlePos n = 
            case find (\(pos, ind) -> pos == n) indices of
                Just (pos, ind) -> 
                    if ind == -1 then
                        tee
                    else
                        hoss
                Nothing -> hos
     in fmap handlePos [start..last-1] <> pure trs

circuitBrujin :: Brujin -> Circuit
circuitBrujin b = case b of
    BLam x -> arrow False $ box False $ wires $ pad 1 0 3 1 $ circuitBrujin x
    BApp x y -> 
        let y' = box True $ pad 0 0 1 1 $ circuitBrujin y
            x' = circuitBrujin x
            (y'', x'') = valign y' x'
         in append 1 (arrow True y'') x'' 
    BInd n -> Circuit [[full, full]] (2, 1) [(0, n)]
    BCon s -> Circuit (pure s) (length s, 1) []
    BQte -> Circuit (pure "#Quote") (6, 1) []