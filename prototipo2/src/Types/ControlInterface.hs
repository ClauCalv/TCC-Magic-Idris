{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- para que instancias das Monads dos transformadores tenham o segundo parâmetro
{-# LANGUAGE FlexibleContexts #-} -- dual ao MultiParamTypeClasses, permite que as constraints tenham o segundo parâmetro
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- só pra poder escrever deriving de Functor e de Applicative automaticamente

module Types.ControlInterface where

import Types.World1

import Control.Monad (forever, when)
import Control.Monad.State (StateT, MonadState(..), evalStateT)
import Control.Monad.Operational (Program, ProgramT, ProgramViewT(..), viewT, liftProgram, singleton)
import Control.Monad.Except (Except, MonadError(..), runExcept)
import Control.Monad.Trans (MonadTrans(..))
import Data.Proxy

import Prelude hiding (print, interact)

-- 'Except b' é só wrapper pra 'Either b', mas achei mais bonito que 'Either b' ou 'ExceptT b Identity'
newtype MagicGame a = MagicGame {runStack :: StateT World (ProgramT Instruction (Except Stop)) a}
    deriving (Functor, Applicative)

-- data SomeSimpleState = SSS Int [(Int, String)]

-- Instruções do programa; interações com as quais o servidor terá que lidar
-- GADT
data Instruction a where
    PrintThisForAll :: String -> Instruction ()
    PrintThis :: Player -> String -> Instruction ()
    ReturnMeThis :: Player -> String -> Question a -> Instruction a

-- Só pra fazer pattern-matching bom no tipo de retorno
-- GADT
data Question a where
    QuestionNumber :: Question Int
    QuestionText :: Question String
    QuestionEnum :: (Bounded a, Enum a) => Proxy a -> Question a
    QuestionSerializable :: (Show a, Read a) => Proxy a -> Question a

-- Condição de parada para o programa
data Stop = UserTypedNegative Int | TotalPlaysReached [(Int, String)]

------ Instancia das classes

-- Não sei porque Control.Monad.Operational.ProgramT não tem uma classe definida.
class Monad m => MonadInteract i m where
    interact :: Program i a -> m a

-- Monad
instance Monad MagicGame where
    return            = MagicGame . return
    MagicGame x >>= f = MagicGame (x >>= (runStack . f))

-- As instancias abaixo e as constraints no final usam MultiParamTypeClasses e FlexibleContexts

-- Primeiro transformador
instance MonadState World MagicGame where
    get = MagicGame get
    put = MagicGame . put

-- Segundo transformador
instance MonadInteract Instruction MagicGame where
    interact = MagicGame . lift . liftProgram

-- Terceiro transformador (usando a classe que criamos pq eles não definem uma)
instance MonadError Stop MagicGame where
    throwError = MagicGame . lift . lift . throwError
    catchError = error "unused"

initialState :: World
initialState = emptyWorld 2

pushInstruction :: Instruction a -> MagicGame a
pushInstruction = interact . singleton

pushStopCondition :: Stop -> MagicGame ()
pushStopCondition = throwError

getState :: MagicGame World
getState = get

putState :: World -> MagicGame ()
putState = put