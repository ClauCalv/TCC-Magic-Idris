{-# LANGUAGE GADTs #-}

-- Nessa primeira etapa só queremos testar comunicação efetiva entre servidor e jogo e o isolamento do IO
-- e a capacidade de descrever um jogo sem ele. Estamos aprendendo a usar stacks de monads e toda a mtl.
-- Criamos um jogo simples para descrever essa comunicação, com elementos significativos: Mensagens que carregam
-- tipo de retorno requerido, interrupção do programa, armazenamento e manutenção de estado, e uma interface clara.

module Server
    ( playGame
    ) where

import Control.Monad (forever, when)
import Control.Monad.State (StateT, MonadState(..), evalStateT)
import Control.Monad.Operational (Program, ProgramT, ProgramViewT(..), viewT, liftProgram, singleton)
import Control.Monad.Except (Except, MonadError(..), runExcept)
import Control.Monad.Trans (MonadTrans(..))

import Prelude hiding (print, interact)
import Text.Read (readMaybe)

import Types.ControlInterface
import Game

playGame :: IO ()
playGame = do
    let program = evalStateT (runStack game) initialState
    runProgram program

------ Executor da pilha de instruções do programa, i.e., o servidor, que contem IO()

runProgram :: ProgramT Instruction (Except Stop) () -> IO ()
-- viewT transforma 'ProgramT i' em 'm (ProgramViewT i m a)' expondo a primeira instrução 'Except Stop (ProgramViewT ...)'
-- runExcept só transforma Except em Either pq eu quis deixar bonitinho
runProgram = evalProg . runExcept . viewT
    where
        -- 'eval prog (Left stopCondition)' deve parar o programa segundo a stopCondition
        evalProg (Left stop) = case stop of
            UserTypedNegative i -> putStrLn $ "Comando inválido, " ++ show i ++ " é um valor negativo."
            TotalPlaysReached xs -> do
                putStrLn "Obrigado por jogar! Eis suas respostas"
                mapM_ putStrLn $ map resultString xs
                putStrLn "Até mais!"
        -- 'eval prog (Right progView)' deve avaliar a instrução do topo e recursivamente chamar a pilha    
        evalProg (Right progView) = case progView of
            Return x -> return x -- não usado por nós, sinaliza fim da pilha
            instr :>>= progGen -> do -- instr é a instrução do topo e progGen é um gerador do resto da pilha dado o resultado de instr
                result <- evalInstr instr -- avalia instr
                runProgram (progGen result) -- recursão no resto da pilha gerado pelo resultado
        
        -- 'evalInstr instruction' deve fazer o que for preciso para retornar o que a instrução demanda
        evalInstr :: Instruction a -> IO a
        evalInstr (PrintThis _ txt) = putStrLn txt
        evalInstr (ReturnMeThis _ txt question) = do
            putStrLn txt
            case question of
                QuestionText -> getLine
                QuestionNumber -> getNumber
                QuestionEnum _ -> getEnum
                QuestionSerializable _ -> getSerializable

getNumber :: IO Int
getNumber = do
  input <- getLine
  case readMaybe input of
    Nothing -> do
      putStrLn "Insira um número inteiro!"
      getNumber
    Just n -> return n

getEnum :: (Bounded a, Enum a) => IO a
getEnum = let limits = (minBound, maxBound) in
    do
        input <- getLine
        case readMaybe input of
            Nothing -> do
                fail limits
            Just n -> if n >= fst limits && n <= snd limits
                then return (toEnum n)
                else fail limits
    where
        fail limits = do 
            putStrLn $ "Insira um número inteiro entre " ++ show (fst limits) ++ " e " ++ show (snd limits) ++ "!"
            getEnum

getSerializable :: (Read a, Show a) => IO a
getSerializable = do
    input <- getLine
    case readMaybe input of
        Nothing -> do
            putStrLn "Valor não pode ser lido"
            getSerializable
        Just n -> return n

resultString :: (Int, String) -> String
resultString (i, txt) = "Answer to " ++ show i ++ ": " ++ txt
