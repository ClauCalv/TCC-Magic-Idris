{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-} -- para que instancias das Monads dos transformadores tenham o segundo parâmetro
{-# LANGUAGE FlexibleContexts #-} -- dual ao MultiParamTypeClasses, permite que as constraints tenham o segundo parâmetro
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- só pra poder escrever deriving de Functor e de Applicative automaticamente

-- Nessa primeira etapa só queremos testar comunicação efetiva entre servidor e jogo e o isolamento do IO
-- e a capacidade de descrever um jogo sem ele. Estamos aprendendo a usar stacks de monads e toda a mtl.
-- Criamos um jogo simples para descrever essa comunicação, com elementos significativos: Mensagens que carregam
-- tipo de retorno requerido, interrupção do programa, armazenamento e manutenção de estado, e uma interface clara.

module V1.Lib
    ( playGame
    ) where

import Control.Monad (forever, when)
import Control.Monad.State (StateT, MonadState(..), evalStateT)
import Control.Monad.Operational (Program, ProgramT, ProgramViewT(..), viewT, liftProgram, singleton)
import Control.Monad.Except (Except, MonadError(..), runExcept)
import Control.Monad.Trans (MonadTrans(..))

import Prelude hiding (print, interact)
import Text.Read (readMaybe)

playGame :: IO ()
playGame = do
    let program = evalStateT (runStack game) initialState
    runProgram program

initialState :: SomeSimpleState
initialState = SSS 0 []

-- 'Except b' é só wrapper pra 'Either b', mas achei mais bonito que 'Either b' ou 'ExceptT b Identity'
newtype MyMonadStack a = MyMonadStack {runStack :: StateT SomeSimpleState (ProgramT Instruction (Except Stop)) a}
    deriving (Functor, Applicative)

data SomeSimpleState = SSS Int [(Int, String)]

-- Instruções do programa; interações com as quais o servidor terá que lidar
-- GADT
data Instruction a where
    PrintThis :: String -> Instruction ()
    ReturnMeThis :: String -> Question a -> Instruction a

-- Só pra fazer pattern-matching bom no tipo de retorno
-- GADT
data Question a where
    QuestionNumber :: Question Int
    QuestionText :: Question String

-- Condição de parada para o programa
data Stop = UserTypedNegative Int | TotalPlaysReached [(Int, String)]

------ Instancia das classes

-- Não sei porque Control.Monad.Operational.ProgramT não tem uma classe definida.
class Monad m => MonadInteract i m where
    interact :: Program i a -> m a

-- Monad
instance Monad MyMonadStack where
    return         = MyMonadStack . return
    MyMonadStack x >>= f = MyMonadStack (x >>= (runStack . f))

-- As instancias abaixo e as constraints no final usam MultiParamTypeClasses e FlexibleContexts

-- Primeiro transformador
instance MonadState SomeSimpleState MyMonadStack where
    get = MyMonadStack get
    put = MyMonadStack . put

-- Segundo transformador
instance MonadInteract Instruction MyMonadStack where
    interact = MyMonadStack . lift . liftProgram

-- Terceiro transformador (usando a classe que criamos pq eles não definem uma)
instance MonadError Stop MyMonadStack where
    throwError = MyMonadStack . lift . lift . throwError
    catchError = error "unused"

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
        evalInstr (PrintThis txt) = putStrLn txt
        evalInstr (ReturnMeThis txt question) = do
            putStrLn txt
            case question of
                QuestionText -> getLine
                QuestionNumber -> getNumber

getNumber :: IO Int
getNumber = do
  input <- getLine
  case readMaybe input of
    Nothing -> do
      putStrLn "Insira um número inteiro!"
      getNumber
    Just n -> return n

resultString :: (Int, String) -> String
resultString (i, txt) = "Answer to " ++ show i ++ ": " ++ txt

------ Construção da pilha de instruções do programa, i.e., o jogo, que não contém IO()

game :: MyMonadStack ()
game = do
    print "Welcome to the \"10 Questions\" Game"
    forever $ do
        number <- askQuestion "Choose a number:" QuestionNumber
        question <- chooseQuestion number
        text <- askQuestion question QuestionText
        saveAnswer number text
    
-- essa e as funções abaixo não precisariam dessas constraints se especializar m
-- i.e. askQuestion :: MonadInteract Instruction m => String -> Question a -> m a
-- vs.  askQuestion :: String -> Question a -> MyMonadStack a
-- Acho que é mais questão de estilo. Não sei qual prefiro.
askQuestion :: MonadInteract Instruction m => String -> Question a -> m a
askQuestion text question = interact $ singleton (ReturnMeThis text question)

print :: MonadInteract Instruction m => String -> m ()
print text = interact $ singleton (PrintThis text)

chooseQuestion :: MonadError Stop m => Int -> m String
chooseQuestion x = do
    when (x < 0) (throwError (UserTypedNegative x))
    return (if even x then "Choose a color:" else "Choose an animal:")

saveAnswer :: (MonadError Stop m, MonadState SomeSimpleState m) => Int -> String -> m ()
saveAnswer number text = do
    (SSS c xs) <- get
    let xs' = (number, text) : xs
    when (c + 1 > 10) (throwError (TotalPlaysReached xs'))
    put (SSS (c + 1) xs')
    

