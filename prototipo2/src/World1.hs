module World1 where

-- NOT WORKING YET

-- Nesta segunda etapa, queremos testar a capacidade de passar e devolver prioridade a dois jogadores distintos,
-- com base em algum estado, se preparando para implementar a pilha de magic e o sistema de prioridades.
-- Também quero ver se consigo começar a usar optics neste trecho e se preparar pra usar em todo o sistema.

data Dict a = Dict Ref [(Ref, a)]
data RefOf a where
    RefOf :: Ref -> RefOf a

data Player = Player
    { name :: String
    , pv :: Int
    }
type Item = String
type Ref = Int

data World = World
    { players       :: Dict Player
    , activePlayer  :: RefOf Player
    , turn          :: (RefOf Player, Int)
    , stack         :: Dict Item
    , battlefield   :: Dict Item
    }

getFromDict :: Dict a -> Ref a -> Maybe a
getFromDict (Dict next dict) (Ref i) = lookup i dict 

updateOnDict :: Dict a -> Ref a -> a -> Dict a
updateOnDict (Dict next dict) (Ref i) x = map (\(j, y)-> if i == j then (i, x) else (j, y)) dict

addToDict :: Dict a -> a -> (Dict a, Ref a)
addToDict (Dict next dict) x -> (Ref next, Dict (next + 1) ((next, x) : dict))

removeFromDict :: Dict a -> Ref a -> Dict a
removeFromDict (Dict next dict) (Ref i) -> Dict next (filter (\(j, y) -> i /= j) dict)