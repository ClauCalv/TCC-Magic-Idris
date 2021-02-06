
-- https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/Types.hs

record World where
  players       :: IdList Player
  activePlayer  :: PlayerRef
  activeStep    :: Step
  time          :: Timestamp
  turnStructure :: List (PlayerRef, List Step)
  exile         :: IdList (ObjectOfType TyCard)
  battlefield   :: IdList (ObjectOfType TyPermanent)
  stack         :: IdList (ObjectOfType TyStackItem)
  command       :: IdList (ObjectOfType TyCard)
  turnHistory   :: List Event

data PriorityAction
  = PlayCard (ObjectRef TyCard)
  | ActivateAbility ActivatedAbilityRef
  --deriving Show

-- Actions that may be taken when paying a mana cost
data PayManaAction
  = PayManaFromManaPool ManaEl
  | ActivateManaAbility ActivatedAbilityRef
