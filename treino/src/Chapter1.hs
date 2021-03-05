{--! run liquid with no-termination -}
module Chapter1 where

average :: [Int] -> Int
average xs = sum xs `div` length xs