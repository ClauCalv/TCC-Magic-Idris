module Magic.Types.Abilities where


-- | Many abilities are run in the context of a source object (carrying the ability) and a player (activating or otherwise controlling it). By making this context explicit, abilities can be run in different contexts, for example by creatures \'stealing\' other creatures\' abilities.
type Contextual a = SomeObjectRef -> PlayerRef -> a

data ActivatedAbility = ActivatedAbility
  { abilityActivation :: Activation
  , abilityType       :: AbilityType
  , tapCost           :: TapCost
  }

data Activation = Activation
  { timing    :: Contextual (View Bool)  -- check timing restrictions
  , available :: Contextual (View Bool)  -- check activator and current zone
  , manaCost  :: Maybe ManaCost
  , effect    :: Contextual (Magic ())
  }

data TapCost = NoTapCost | TapCost  -- add later: UntapCost

data AbilityType = ActivatedAb | ManaAb | LoyaltyAb
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type StackItem = TargetList (ObjectRef TyStackItem -> PlayerRef -> Magic ())

data StaticKeywordAbility
  = Bloodthirst Int
  | Deathtouch
  | Defender
  | DoubleStrike
  | EnchantPermanent ObjectTypes
  | FirstStrike
  | Flash
  | Flashback ManaCost
  | Flying
  | Haste
  | Hexproof
  | Infect
  | Intimidate
  | Lifelink
  | ProtectionFromColor Color
  | Reach
  | Shroud
  | Trample
  | Vigilance
  deriving (Eq, Ord, Show, Read)

-- | A layered effect affects a set of objects and applies one or more
-- modifications to them. The order in which the effects are applied is
-- managed by layers [613]. By separating the affected objects from the
-- modifications, we can detect dependencies [613.7].
data LayeredEffect
  = LayeredObjectEffect
      { affectedObjects     :: Contextual (View [SomeObjectRef])
      , objectModifications :: [ModifyObject]
      }

instance Show LayeredEffect where
  show _ = "(layered effect)"

-- | Temporary layered effects are created by the resolution of instants,
-- sorceries and activated abilities.
data TemporaryLayeredEffect = TemporaryLayeredEffect
  { temporaryTimestamp :: Timestamp
  , temporaryDuration  :: Duration
  , temporaryEffect    :: LayeredEffect
  }

instance Show TemporaryLayeredEffect where
  show _ = "(temporary layered effect)"

-- | Modifications of objects that are part of layered effects.
data ModifyObject
  = ChangeController PlayerRef
  | ChangeTypes (ObjectTypes -> ObjectTypes)
  | ChangeColors (Set Color -> Set Color)
  | AddStaticKeywordAbility StaticKeywordAbility
  | RemoveStaticKeywordAbility StaticKeywordAbility
  | AddActivatedAbility ActivatedAbility
  | AddTriggeredAbilities TriggeredAbilities
  | RemoveAllAbilities
  | DefinePT (SomeObjectRef -> View PT)
  | SetPT PT
  | ModifyPT (SomeObjectRef -> View PT)
  | SwitchPT
  | RestrictAllowAttacks ([Attack] -> Contextual (View Bool))
  | RestrictAllowBlocks ([Block] -> Contextual (View Bool))

-- | Layers in which a layered effect can apply.
data Layer
  = Layer1       -- ^ Copy effects
  | Layer2       -- ^ Control-changing effects
  | Layer3       -- ^ Text-changing effects
  | Layer4       -- ^ Type-changing effects
  | Layer5       -- ^ Color-changing effects
  | Layer6       -- ^ Ability-adding and ability-removing effects
  | Layer7a      -- ^ Characteristic-defining abilities that set P/T
  | Layer7b      -- ^ Effects that set P/T
  | Layer7c      -- ^ Effects that modify P/T
  | Layer7d      -- ^ P/T counters
  | Layer7e      -- ^ Effects that switch p/t
  | LayerPlayer  -- ^ Player-affecting effects
  | LayerRules   -- ^ Rules-affecting effects
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Duration with which a 'TemporaryLayeredEffect' can apply.
data Duration
  = Indefinitely
  | UntilEndOfTurn
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type ReplacementEffect =
  OneShotEffect -> Contextual (Maybe (Magic [OneShotEffect]))

type TriggeredAbilities = [Event] -> Contextual (View [Magic ()])
