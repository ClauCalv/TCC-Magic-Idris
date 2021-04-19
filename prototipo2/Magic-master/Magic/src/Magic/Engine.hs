{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module Magic.Engine (newWorld, fullGame, Engine(..)) where

import Magic.Core
import Magic.Events hiding (executeEffect, executeEffects)
import qualified Magic.IdList as IdList
import Magic.Labels
import Magic.ObjectTypes
import Magic.Predicates
import Magic.Target
import Magic.Types
import Magic.Utils
import Magic.Engine.Types
import Magic.Engine.Events
import Magic.Some

import Control.Applicative ((<$>), (<*>))
import Control.Category ((.))
import Control.Monad (forever, forM_, when, void, liftM, unless)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (tell, execWriterT)
import Data.Boolean ((&&*))
import Data.Label (get, set, modify)
import Data.Label.Monadic (gets, (=:), (=.), asks)
import Data.List (nub, intersect, delete)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import qualified Data.MultiSet as MultiSet
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Traversable (for)
import Prelude hiding (round, (.))



newWorld :: [Deck] -> World
newWorld decks = World
    { _players       = ps
    , _activePlayer  = head playerIds
    , _activeStep    = BeginningPhase UntapStep
    , _time          = 0
    , _turnStructure = ts
    , _exile         = IdList.empty
    , _battlefield   = IdList.empty
    , _stack         = IdList.empty
    , _command       = IdList.empty
    , _turnHistory   = []
    }
  where
    ps = IdList.fromListWithId (\i deck -> newPlayer i deck) decks
    playerIds = IdList.ids ps
    ts = case playerIds of
          -- 103.7a. In a two-player game, the player who plays first skips the draw step of his or her first turn.
          [p1, p2] -> (p1, removeFirst (== BeginningPhase DrawStep) turnSteps)
                        : cycle [(p2, turnSteps), (p1, turnSteps)]
          _        -> cycle (map (, turnSteps) playerIds)

removeFirst :: (a -> Bool) -> [a] -> [a]
removeFirst notOk xs =
  case break notOk xs of
    (as, _ : bs) -> as ++ bs
    _ -> xs

turnSteps :: [Step]
turnSteps =
  [ BeginningPhase UntapStep
  , BeginningPhase UpkeepStep
  , BeginningPhase DrawStep
  , MainPhase
  , CombatPhase BeginningOfCombatStep
  , CombatPhase DeclareAttackersStep
  , CombatPhase DeclareBlockersStep
  , CombatPhase CombatDamageStep
  , CombatPhase EndOfCombatStep
  , MainPhase
  , EndPhase EndOfTurnStep
  , EndPhase CleanupStep
  ]

newPlayer :: PlayerRef -> Deck -> Player
newPlayer i deck = Player
  { _life = 20
  , _manaPool = mempty
  , _prestack = []
  , _library = IdList.fromList [ CardObject (instantiateCard card i)
                               | card <- deck ]
  , _hand = IdList.empty
  , _graveyard = IdList.empty
  , _maximumHandSize = Just 7
  , _failedCardDraw = False
  }

drawOpeningHands :: [PlayerRef] -> Int -> Engine ()
drawOpeningHands [] _ =
  return ()
drawOpeningHands playerIds 0 =
  void $ executeEffects TurnBasedActions (map (Will . ShuffleLibrary) playerIds)
drawOpeningHands playerIds handSize = do
  mulliganingPlayers <- do
    forM_ playerIds $ \playerId -> do
      _ <- undrawHand TurnBasedActions playerId
      _ <- executeEffect TurnBasedActions (Will (ShuffleLibrary playerId))
      executeEffects TurnBasedActions (replicate handSize (Will (DrawCard playerId)))
    for playerIds $ \playerId -> do
      keepHand <- askQuestion playerId AskKeepHand
      if keepHand
        then return Nothing
        else return (Just playerId)
  drawOpeningHands (catMaybes mulliganingPlayers) (handSize - 1)

undrawHand :: EventSource -> PlayerRef -> Engine [Event]
undrawHand source p = do
  idxs <- IdList.toList <$> gets (hand . player p)
  executeEffects source [ WillMoveObject (Just (Some (Hand p), i)) (Library p) x | (i, x) <- idxs ]

fullGame :: Engine ()
fullGame = do
  ps <- IdList.ids <$> gets players
  drawOpeningHands ps 7
  forever $ do
    players =.* set manaPool mempty
    (step, newTurn) <- nextStep
    when newTurn (turnHistory =: [])
    raise TurnBasedActions [DidBeginStep step]
    executeStep step
    raise TurnBasedActions [WillEndStep step]

-- | Moves to the next step. Returns the new step, and whether a new turn has begun.
nextStep :: Engine (Step, Bool)
nextStep = do
  structure <- gets turnStructure
  case structure of
    (rp, s : ss) : ts -> do
      turnStructure =: if null ss then ts else (rp, ss) : ts
      activePlayer  =: rp
      activeStep    =: s
      return (s, null ss)



-- Execution of steps

executeStep :: Step -> Engine ()

executeStep (BeginningPhase UntapStep) = do
  -- TODO [502.1]  phasing

  -- [502.2] untap permanents
  rp <- gets activePlayer
  ios <- (IdList.filter (\perm@Permanent {} -> isControlledBy rp (get objectPart perm))) <$> gets battlefield
  _ <- executeEffects TurnBasedActions (map (\(i, _) -> Will (UntapPermanent (Battlefield, i))) ios)
  return ()

executeStep (BeginningPhase UpkeepStep) = do
  -- TODO [503.1]  handle triggers

  -- [503.2]
  offerPriority

executeStep (BeginningPhase DrawStep) = do
  -- [504.1]
  ap <- gets activePlayer
  _ <- executeEffect TurnBasedActions (Will (DrawCard ap))

  -- TODO [504.2]  handle triggers

  -- [504.3]
  offerPriority

executeStep MainPhase = do
  -- TODO [505.4]  handle triggers

  -- [505.5]
  offerPriority

executeStep (CombatPhase BeginningOfCombatStep) = do
  offerPriority

executeStep (CombatPhase DeclareAttackersStep) = do
  -- [508.1a] declare attackers
  -- [508.1b] declare which player or planeswalker each attacker attacks
  -- TODO Offer attacking planeswalkers
  -- TODO Check summoning sickness

  ap <- gets activePlayer
  let canAttack perm =
        (isControlledBy ap &&* hasTypes creatureType) (get objectPart perm)
        && get tapStatus perm == Untapped
  possibleAttackerRefs <- map (\(i,_) -> (Battlefield, i)) . filter (canAttack . snd) . IdList.toList <$> view (asks battlefield)
  attackablePlayerRefs <- (filter (/= ap) . IdList.ids) <$> gets players
  attacks <- askQuestion ap (AskAttackers possibleAttackerRefs (map PlayerRef attackablePlayerRefs))

  -- TODO [508.1c] check attacking restrictions
  -- TODO [508.1d] check attacking requirements
  -- TODO [508.1e] declare banding

  -- [508.1f] tap attackers
  forM_ attacks $ \(Attack rAttacker _rAttackee) -> do
    keywords <- view (asks (staticKeywordAbilities . objectPart . object rAttacker))
    unless (elem Vigilance keywords) $
        tapStatus . object rAttacker =: Tapped
  -- TODO [508.1g] determine costs
  -- TODO [508.1h] allow mana abilities
  -- TODO [508.1i] pay costs

  -- [508.1j] mark creatures as attacking
  forM_ attacks $ \(Attack rAttacker rAttackee) ->
    attacking . object rAttacker =: Just rAttackee

  -- [508.2]  handle triggers
  raise TurnBasedActions [DidDeclareAttackers ap attacks]

  offerPriority
  -- TODO [508.6]  potentially skip declare blockers and combat damage steps
  return ()

executeStep (CombatPhase DeclareBlockersStep) = do
  -- TODO [509.1a] declare blockers
  -- TODO [509.1b] check blocking restrictions
  -- TODO [509.1c] check blocking requirements
  -- TODO [509.1d] determine costs
  -- TODO [509.1e] allow mana abilities
  -- TODO [509.1f] pay costs
  -- TODO [509.1g] mark creatures as blocking
  -- TODO [509.1h] mark creatures as blocked
  -- TODO [509.2]  declare attackers' damage assignment order
  -- TODO [509.3]  declare blockers' damage assignment order
  -- TODO [509.4]  handle triggers
  offerPriority
  -- TODO [509.6]  determine new attackers' damage assignment order
  -- TODO [509.7]  determine new blockers' damage assignment order
  return ()

executeStep (CombatPhase CombatDamageStep) = do
  -- TODO [510.1]  assign combat damage
  -- TODO [510.2]  deal damage
  -- TODO [510.3]  handle triggers
  effectses <- view $ do
    permanents <- IdList.toList <$> asks battlefield
    for permanents $ \(r, permanent) -> do
      let attackingObject = _permanentObject permanent
      let Just (power, _) = _pt attackingObject
      case _attacking permanent of
        Nothing -> return []
        Just (PlayerRef p) -> return [Will (DamagePlayer attackingObject p power True True)]
        Just (ObjectRef (Some Battlefield, i)) -> return [Will (DamageObject attackingObject (Battlefield, i) power True True)]
  _ <- executeEffects TurnBasedActions (concat effectses)
  offerPriority
  -- TODO [510.5]  possibly introduce extra combat damage step for first/double strike
  return ()

executeStep (CombatPhase EndOfCombatStep) = do
  -- TODO [511.1]  handle triggers

  -- [511.2]
  offerPriority

  -- [511.3]  remove creatures from combat
  battlefield =.* set attacking Nothing

executeStep (EndPhase EndOfTurnStep) = do
  -- TODO [513.1]  handle triggers
  
  -- [513.2]
  offerPriority

executeStep (EndPhase CleanupStep) = do
  -- TODO [514.1]  discard excess cards
  -- [514.2] remove damage from permanents
  battlefield =.* set damage 0

  -- [514.2] Remove effects that last until end of turn
  battlefield =.* modify (temporaryEffects . objectPart)
    (filter (\tle -> temporaryDuration tle /= UntilEndOfTurn))
  shouldOfferPriority <- executeSBAsAndProcessPrestacks
  when shouldOfferPriority offerPriority






offerPriority :: Engine ()
offerPriority = gets activePlayer >>= fullRoundStartingWith
  where
    fullRoundStartingWith p = do
      _ <- executeSBAsAndProcessPrestacks
      mAction <- playersStartingWith p >>= partialRound
      case mAction of
        Just (initiatingPlayer, action) -> do
          executePriorityAction initiatingPlayer action
          fullRoundStartingWith initiatingPlayer
        Nothing -> do
          st <- gets stack
          case IdList.head st of
            Nothing -> return ()
            Just (i, _) -> do
              resolve (Stack, i)
              offerPriority

    partialRound ((p, _):ps) = do
      actions <- collectPriorityActions p
      mAction <- askQuestion p (AskPriorityAction actions)
      case mAction of
        Just action -> return (Just (p, action))
        Nothing -> partialRound ps
    partialRound [] = return Nothing

-- | Repeatedly checks and execute state-based effects and asks players to put triggered abilities
--   on the stack, until everything has been processed. Returns whether any SBAs have been taken or
--   whether any abilities have been put on the stack.
executeSBAsAndProcessPrestacks :: Engine Bool
executeSBAsAndProcessPrestacks = untilFalse ((||) <$> checkSBAs <*> processPrestacks)

-- | Repeatedly checks and executes state-based effects until no more actions need to be taken.
--   Returns whether any actions were taken at all.
checkSBAs :: Engine Bool
checkSBAs = untilFalse $ (not . null) <$> checkSBAsOnce
  where
    checkSBAsOnce = collectSBAs >>= executeEffects StateBasedActions

-- | Ask players to put pending items on the stack in APNAP order. [405.3]
--   Returns whether anything was put on the stack as a result.
processPrestacks :: Engine Bool
processPrestacks = do
  ips <- apnap
  liftM or $ for ips $ \(i,p) -> do
    let pending = get prestack p
    when (not (null pending)) $ do
      index <- askQuestion i (AskPickTrigger (map fst pending))
      let (lki, program) = pending !! index
      executeMagic (StackTrigger lki) program
      prestack . player i =. deleteAtIndex index
    return (not (null pending))

untilFalse :: Monad m => m Bool -> m Bool
untilFalse p = do
  b <- p
  if b
    then untilFalse p >> return True
    else return False

-- | Collects and returns all applicable state-based actions (without executing them).
collectSBAs :: Engine [OneShotEffect]
collectSBAs = view $ execWriterT $ do
    checkPlayers
    checkBattlefield
    -- TODO [704.5d]
    -- TODO [704.5e]
    -- TODO [704.5u]
    -- TODO [704.5v]
    -- TODO [704.5w]
  where
    checkPlayers = do
      -- [704.5a]
      -- [704.5b]
      -- TODO [704.5c]
      -- TODO [704.5t]
      ips <- IdList.toList <$> lift (asks players)
      forM_ ips $ \(i,p) -> do
        when (get life p <= 0 || get failedCardDraw p) $
          tell [Will (LoseGame i)]

    checkBattlefield = do
      ios <- IdList.toList <$> lift (asks battlefield)
      forM_ ios $ \(i, Permanent o _ dam deatht _ _) -> do

        -- Check creatures
        when (hasTypes creatureType o) $ do

          -- [704.5f]
          let hasNonPositiveToughness = maybe False (<= 0) (fmap snd (get pt o))
          when hasNonPositiveToughness $ tell [willMoveToGraveyard (Battlefield, i) o]

          -- [704.5g]
          -- [704.5h]
          let hasLethalDamage =
                case get pt o of
                  Just (_, t) -> t > 0 && dam >= t
                  _           -> False
          when (hasLethalDamage || deatht) $
            tell [Will (DestroyPermanent (Battlefield, i) True)]

        -- [704.5i]
        when (hasTypes planeswalkerType o && countCountersOfType Loyalty o == 0) $
          tell [willMoveToGraveyard (Battlefield, i) o]

      -- TODO [704.5j]
      -- TODO [704.5k]
      -- TODO [704.5m]
      -- TODO [704.5n]
      -- TODO [704.5p]
      -- TODO [704.5q]
      -- TODO [704.5r]
      -- TODO [704.5s]

resolve :: ObjectRef TyStackItem -> Engine ()
resolve r@(Stack, i) = do
  stackItem <- gets (object r)
  case stackItem of
    StackItem o item -> do
      let (_, Just mkEffects) = evaluateTargetList item
      let eventSource = ResolutionOf r
      executeMagic eventSource (mkEffects r (get controller o))

      -- if the object is now still on the stack, move it to the appropriate zone
      if (hasTypes instantType o || hasTypes sorceryType o)
      then void $ executeEffect eventSource $
        WillMoveObject (Just (Some Stack, i)) (Graveyard (get controller o)) (CardObject o)
      else if hasPermanentType o
      then void $ executeEffect eventSource $
        WillMoveObject (Just (Some Stack, i)) Battlefield (Permanent o Untapped 0 False Nothing Nothing)
      else void $ executeEffect eventSource $ Will $ CeaseToExist (Some Stack, i)

collectPriorityActions :: PlayerRef -> Engine [PriorityAction]
collectPriorityActions p = do
  as <- map ActivateAbility <$> collectAvailableActivatedAbilities (const True) p
  plays <- map PlayCard <$> collectPlayableCards p
  return (as <> plays)

collectAvailableActivatedAbilities :: (ActivatedAbility -> Bool) -> PlayerRef -> Engine [ActivatedAbilityRef]
collectAvailableActivatedAbilities predicate p = do
  objects <- view allObjects
  execWriterT $ do
    for objects $ \(r,o) -> do
      for (zip [0..] (get activatedAbilities o)) $ \(i, ability) -> do
        ok <- lift (shouldOfferActivation (abilityActivation ability) r p)
        payCostsOk <- lift (canPayTapCost (tapCost ability) r p)
        when (predicate ability && ok && payCostsOk) (tell [(r, i)])

collectPlayableCards :: PlayerRef -> Engine [ObjectRef TyCard]
collectPlayableCards p = do
  objects <- view allCards
  execWriterT $ do
    forM_ objects $ \(r,o) -> do
      case get play o of
        Just playAbility -> do
          ok <- lift (shouldOfferActivation playAbility (someObjectRef r) p)
          when ok (tell [r])
        Nothing -> return ()

shouldOfferActivation :: Activation -> Contextual (Engine Bool)
shouldOfferActivation activation rSource you =
  view ((timing &&* available) activation rSource you)

activate :: EventSource -> Activation -> Contextual (Engine ())
activate source activation rSource rActivator  = do
  --case manaCost activation of
  --  Just mc -> offerManaAbilitiesToPay source rActivator mc
  --  Nothing -> return ()
  executeMagic source (effect activation rSource rActivator)

executePriorityAction :: PlayerRef -> PriorityAction -> Engine ()
executePriorityAction p a = do
  case a of
    PlayCard r -> do
      maybeAbility <- gets (play . objectPart . object r)
      case maybeAbility of
        Just ability ->
          activate (PriorityActionExecution a) ability (someObjectRef r) p
    ActivateAbility (r, i) -> do
      abilities <- gets (activatedAbilities . objectBase r)
      let ab = abilities !! i
      let eventSource = PriorityActionExecution a
      payTapCost eventSource (tapCost ab) r p
      activate eventSource (abilityActivation ab) r p

offerManaAbilitiesToPay :: EventSource -> PlayerRef -> ManaCost -> Engine ()
offerManaAbilitiesToPay _ _ cost | MultiSet.null cost  = return ()
offerManaAbilitiesToPay source p cost = do
  amas <- map ActivateManaAbility <$>
          collectAvailableActivatedAbilities ((== ManaAb) . abilityType) p
  pool <- gets (manaPool . player p)
  let pms =
        if MultiSet.member GenericCost cost
        then
          -- There is at least 1 generic mana to pay.
          -- Offer all colors in the mana pool to spend.
          map PayManaFromManaPool (MultiSet.distinctElems pool)
        else
          -- Cost has no generic component.
          -- Only offer colors in the mana pool that occur in the cost.
          [ PayManaFromManaPool manaEl
          | ManaElCost manaEl <- MultiSet.distinctElems cost
          , manaEl `MultiSet.member` pool
          ]
  action <- askQuestion p (AskManaAbility cost (amas <> pms))
  case action of
    PayManaFromManaPool manaEl -> do
      _ <- executeEffect source $
        Will (SpendFromManaPool p (MultiSet.singleton manaEl))
      let restCost =
            -- Pay a colored cost element if possible;
            -- otherwise pay a generic element.
            if ManaElCost manaEl `elem` cost
            then MultiSet.delete (ManaElCost manaEl) cost
            else MultiSet.delete GenericCost         cost
      offerManaAbilitiesToPay source p restCost
    ActivateManaAbility (r, i) -> do
      abilities <- gets (activatedAbilities . objectBase r)
      activate source (abilityActivation (abilities !! i)) r p
      offerManaAbilitiesToPay source p cost

canPayTapCost :: TapCost -> Contextual (Engine Bool)
canPayTapCost NoTapCost _ _ = return True
canPayTapCost TapCost (Some Battlefield, i) _ =
  (== Untapped) <$> gets (tapStatus . object (Battlefield, i))
canPayTapCost TapCost _ _ = return False

payTapCost :: EventSource -> TapCost -> Contextual (Engine ())
payTapCost _ NoTapCost _ _ = return ()
payTapCost source TapCost (Some Battlefield, i) _ =
  void (executeEffect source (Will (TapPermanent (Battlefield, i))))
payTapCost _ _ _ _ = return ()

-- | Returns player IDs in APNAP order (active player, non-active player).
apnap :: Engine [(PlayerRef, Player)]
apnap = gets activePlayer >>= playersStartingWith

playersStartingWith :: PlayerRef -> Engine [(PlayerRef, Player)]
playersStartingWith p = do
  (ps, qs) <- break ((== p) . fst) . IdList.toList <$> gets players
  return (qs ++ ps)
