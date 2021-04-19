module Magic.Types.Events where

-- | Events are caused by various actions in the game. They describe something that has just happened, such as executing a 'OneShotEffect', progressing to the next step or phases, casting spells, et cetera. Events form the input for triggered abilities.
data Event
  = Did SimpleOneShotEffect
  | DidMoveObject (Maybe SomeObjectRef) SomeObjectRef  -- old ref, new ref
  | DidDeclareAttackers PlayerRef [Attack]

  -- Keyword actions [701]
  | DidActivateAbility SomeObjectRef Int  -- index of ability
  | DidCounter (ObjectRef TyStackItem) (ObjectRef TyStackItem)  -- source (spell or ability), target
  | DidBeginStep Step
  | WillEndStep Step
  deriving Show

-- | A one-shot effect causes a mutation in the game's state. A value of @OneShotEffect@ describes something that is about to happen. When one-shot effects are executed, they may be replaced or prevented by replacement effects, and cause an 'Event' to be raised, triggering abilities.
data OneShotEffect
  = Will SimpleOneShotEffect
  | forall ty. WillMoveObject (Maybe SomeObjectRef) (ZoneRef ty) (ObjectOfType ty)  -- optional current zone/id, new zone, suggested form

deriving instance Show OneShotEffect

-- | A one-shot effect is simple if its fields contain enough information to serve as an 'Event' unchanged, using the 'Did' constructor.
data SimpleOneShotEffect
  = GainLife PlayerRef Int
  | LoseLife PlayerRef Int
  | DamageObject Object (ObjectRef TyPermanent) Int Bool Bool  -- source, creature/planeswalker, amount, combat damage?, preventable?
  | DamagePlayer Object PlayerRef Int Bool Bool  -- source, player, amount, combat damage?, preventable?
  | ShuffleLibrary PlayerRef
  -- ReorderLibraryCards
  | DrawCard PlayerRef -- Drawing is special [120.5]
  | DestroyPermanent (ObjectRef TyPermanent) Bool  -- object on battlefield, regenerate allowed?
  | TapPermanent (ObjectRef TyPermanent)  -- object on battlefield
  | UntapPermanent (ObjectRef TyPermanent)  -- object on battlefield
  | AddCounter SomeObjectRef CounterType
  | RemoveCounter SomeObjectRef CounterType
  | AddToManaPool PlayerRef ManaPool
  | SpendFromManaPool PlayerRef ManaPool
  | AttachPermanent (ObjectRef TyPermanent) (Maybe SomeObjectRef) (Maybe SomeObjectRef)  -- aura/equipment, old target, new target
  | RemoveFromCombat (ObjectRef TyPermanent)
  | PlayLand PlayerRef SomeObjectRef
  | LoseGame PlayerRef
  | WinGame PlayerRef
  | InstallLayeredEffect SomeObjectRef TemporaryLayeredEffect
  | CeaseToExist SomeObjectRef
  | Sacrifice (ObjectRef TyPermanent)
  | RevealCards PlayerRef [ObjectRef TyCard]
  deriving Show

-- | A creature attacking a player or a planeswalker.
data Attack = Attack
  { -- | The creature that is attacking.
    attacker :: ObjectRef TyPermanent
    -- | The player or planeswalker being attacked.
  , attackee :: EntityRef
  } deriving Show

-- | A creature blocking another creature.
data Block = Block
  { -- | The creature blocking.
    blocker :: ObjectRef TyPermanent
    -- | The creature being blocked.
  , blockee :: ObjectRef TyPermanent
  }