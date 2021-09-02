{-# LANGUAGE GADTs #-}

module Data.Dict where

import Prelude hiding (map, filter, null)
import qualified Data.List as L
import Data.Maybe

type Ref = Int

data Dict a = Dict (RefOf a) [(RefOf a, a)]

data RefOf a where
    Ref :: Ref -> RefOf a
    deriving Eq

empty :: Dict a
empty = Dict (Ref 0) []

get :: Dict a -> RefOf a -> Maybe a
get (Dict _ elems) ref = lookup ref elems 

set :: Dict a -> RefOf a -> a -> Dict a
set dict ref x = map set' dict
    where set' r y = if r == ref then x else y

put :: Dict a -> a -> (RefOf a, Dict a)
put (Dict r elems) x = (r, Dict (next r) ((r, x) : elems))
    where next (Ref i) = Ref (i+1)

remove :: Dict a -> RefOf a -> Dict a
remove dict r = filter keep' dict
    where keep' ref _ = r /= ref

map :: (RefOf a -> a -> a) -> Dict a -> Dict a
map f (Dict next elems) = Dict next (L.map f' elems)
    where f' (r, x) = (r, f r x)

modify :: (RefOf a -> a -> Maybe a) -> Dict a -> Dict a
modify f (Dict next elems) = Dict next elems'
    where
        elems' = L.map fromJust' $ L.filter isJust' $ L.map f' elems
        isJust' (a, b) = isJust b
        fromJust' (a, Just b) = (a, b)
        f' (r, x) = (r, f r x)

filter :: (RefOf a -> a -> Bool) -> Dict a -> Dict a
filter f (Dict next elems) = Dict next (L.filter f' elems)
    where f' (r, x) = f r x

elems :: Dict a -> [(RefOf a, a)]
elems (Dict _ xs) = xs

null :: Dict a -> Bool
null (Dict _ xs) = L.null xs

splitPop :: Dict a -> ((RefOf a, a), Dict a)
splitPop = error "TODO"