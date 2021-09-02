module Game (game) where

import Control.Monad (forever, when)
import Data.Proxy

import Prelude hiding (print, interact)
import Optics

import Types.ControlInterface
import Types.World1

import qualified Data.Dict as D

data PriorityAction = Pass | AskGameState | TakeAction PlayerAction deriving (Eq, Show, Read)
data PlayerAction = CreateObject String Int | DestroyObject Int deriving (Eq, Show, Read)

------ Construção da pilha de instruções do programa, i.e., o jogo, que não contém IO()

game :: MagicGame ()
game = do
    print "Starting game ..."
    forever $ do
        doRound

doRound :: MagicGame ()
doRound = do
    initRound
    p <- use priorityPlayer
    runPriorityActions p
    performAttacks
    endRound

initRound :: MagicGame ()
initRound = do
    ps <- use players
    (playerRef, count) <- use turn
    let Just player = D.get ps playerRef -- needs LH to prove safety
    print $ "Round " ++ show count ++ ", player " ++ view name player
    assign priorityPlayer playerRef

runPriorityActions :: D.RefOf Player -> MagicGame ()
runPriorityActions initialPlayer = do
    pp <- use priorityPlayer
    action <- askPriorityAction pp
    case action of
        Pass -> do
            nextPlayer <- findNextPlayer
            if nextPlayer == initialPlayer
                then do
                    stack <- resolveStack
                    if D.null stack
                        then return ()
                        else do
                            p <- use (turn % _1)
                            assign priorityPlayer p
                            runPriorityActions p
                else do
                    assign priorityPlayer nextPlayer
                    runPriorityActions initialPlayer
        TakeAction x -> do
            resolveThatAction x
            runPriorityActions pp
        AskGameState -> do
            world <- getState
            print $ show world
            runPriorityActions initialPlayer

performAttacks :: MagicGame ()
performAttacks = return ()

endRound :: MagicGame ()
endRound = do
    (playerRef, count) <- use turn
    nextPlayer <- findNextPlayer
    assign turn (nextPlayer, count + 1)

findNextPlayer :: MagicGame (D.RefOf Player)
findNextPlayer = do
    ps <- use players
    (D.Ref i) <- use priorityPlayer
    let gtelems = D.elems $ D.filter (\(D.Ref r) x -> r > i) ps
    case gtelems of
        (p : _) -> return $ fst p
        [] -> return . fst . head $ D.elems ps

resolveStack :: MagicGame (D.Dict Item)
resolveStack = do
    st <- use stack
    bf <- use battlefield
    let (stHead, stTail) = D.splitPop st
    assign stack stTail
    assign battlefield $ snd (D.put bf (snd stHead))
    return stTail

resolveThatAction :: PlayerAction -> MagicGame ()
resolveThatAction action = case action of
    CreateObject name dmg -> do
        print "TODO"
        return ()
    DestroyObject id -> do
        print "TODO"
        return ()

askPriorityAction :: D.RefOf Player -> MagicGame PriorityAction
askPriorityAction p = do
    ps <- use players
    let Just player = D.get ps p
    askQuestion player "Choose a priority action" $ QuestionSerializable (Proxy :: Proxy PriorityAction)

askQuestion :: Player -> String -> Question a -> MagicGame a
askQuestion p text question = pushInstruction (ReturnMeThis p text question)

print :: String -> MagicGame ()
print text = pushInstruction (PrintThisForAll text)

sendMessage :: Player -> String -> MagicGame ()
sendMessage p text = pushInstruction (PrintThis p text)

chooseQuestion :: Int -> MagicGame String
chooseQuestion x = do
    when (x < 0) (pushStopCondition (UserTypedNegative x))
    return (if even x then "Choose a color:" else "Choose an animal:")

saveAnswer :: Int -> String -> MagicGame ()
saveAnswer number text = do
    -- (SSS c xs) <- getState
    -- let xs' = (number, text) : xs
    -- when (c + 1 > 10) (pushStopCondition (TotalPlaysReached xs'))
    -- putState (SSS (c + 1) xs')
    return ()
    

