module Lib
    ( someFunc
    ) where

import Magic.Data.UniqueDict
import Magic.Types.World

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-@ type Positive = {v:Int | 0 < v} @-}

-- {-@ nonPositive :: Positive @-}
-- nonPositive = -1 --should give an error. -- it gives!
