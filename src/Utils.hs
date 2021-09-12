module Utils where

import Data.List(foldl')

findLast :: (a -> Bool) -> [a] -> Maybe a
findLast p = foldl' (\b a -> if p a then Just a else b) Nothing

bracket :: String -> String
bracket s = "(" <> s <> ")"