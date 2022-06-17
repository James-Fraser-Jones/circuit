module Utils where

import Data.List

findLast :: (a -> Bool) -> [a] -> Maybe a
findLast p = foldl' (\b a -> if p a then Just a else b) Nothing

bracket :: String -> String
bracket s = "(" <> s <> ")"

format :: Show b => Int -> Int -> (a -> [b]) -> a -> String
format maxIterations newlines norm term =
  let reductions = norm term
      iterations = take maxIterations reductions
      iter_strings = map show iterations
      continued_strings = iter_strings ++ pure (if min_length reductions (maxIterations + 1) then "... reduction continues ..." else "")
   in intercalate (replicate newlines '\n') continued_strings

--lazily calculates whether list is at least n long
min_length :: [a] -> Int -> Bool
min_length [] n = n <= 0
min_length (x:xs) n = 
  if n <= 0 then True
  else min_length xs (n - 1)