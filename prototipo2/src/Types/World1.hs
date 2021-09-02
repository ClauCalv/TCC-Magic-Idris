{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.World1 where

-- NOT WORKING YET

-- Nesta segunda etapa, queremos testar a capacidade de passar e devolver prioridade a dois jogadores distintos,
-- com base em algum estado, se preparando para implementar a pilha de magic e o sistema de prioridades.
-- Também quero ver se consigo começar a usar optics neste trecho e se preparar pra usar em todo o sistema.

import qualified Data.Dict as D

import Optics
import Optics.TH

data Player = Player
    { _name :: String
    , _pv :: Int
    }

data Item = Item
    { _itemname :: String
    , _attack :: Int
    , _owner :: D.RefOf Player   
    }

data World = World
    { _players           :: D.Dict Player
    , _priorityPlayer    :: D.RefOf Player
    , _turn              :: (D.RefOf Player, Int)
    , _stack             :: D.Dict Item
    , _battlefield       :: D.Dict Item
    }

makeLenses ''Player
makeLenses ''Item
makeLenses ''World

makePrisms ''Player
makePrisms ''Item
makePrisms ''World

instance Show World where
    show _ = "TODO"

emptyWorld :: Int -> World
emptyWorld p = World 
    { _players           = putPlayer p D.empty
    , _priorityPlayer    = D.Ref 0
    , _turn              = (D.Ref 0, 0)
    , _stack             = D.empty
    , _battlefield       = D.empty
    }
    where
        putPlayer 0 dict = dict
        putPlayer n dict = putPlayer (n - 1) $ snd (D.put dict (newPlayer n))
        newPlayer n = Player ("Player " ++ show (p - n + 1)) 20
