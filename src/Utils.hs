module Utils where

import Data.List

findLast :: (a -> Bool) -> [a] -> Maybe a
findLast p = foldl' (\b a -> if p a then Just a else b) Nothing

bracket :: String -> String
bracket s = "(" <> s <> ")"

format :: Show b => Int -> Int -> (a -> [b]) -> a -> String
format maxIterations newlines norm = concat . intersperse (replicate newlines '\n') . map show . take maxIterations . norm
