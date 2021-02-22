module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-@ type Positive = {v:Int | 0 < v} @-}

{-@ nonPositive :: Positive @-}
nonPositive = -1 --should give an error.
