
-- https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/Types.hs

-- ABILITIES

-- Todas essas habilidades são de um conjunto específico, no caso somente o M13. Isso teria que ser mudado para incluir mais conjuntos de cartas.
-- A mesma coisa sobre o arquivo ObjectsTypes.

-- | Many abilities are run in the context of a source object (carrying the ability) and a player (activating or otherwise controlling it). By making this context explicit, abilities can be run in different contexts, for example by creatures \'stealing\' other creatures\' abilities.
Contextual : Type -> Type
Contextual a = SomeObjectRef -> PlayerRef -> a


-- ActivatedAbility
record ActivatedAbility where
  abilityActivation : Activation
  abilityType       : AbilityType
  tapCost           : TapCost

record Activation where
  timing    : Contextual (View Bool)  -- check timing restrictions
  available : Contextual (View Bool)  -- check activator and current zone
  manaCost  : Maybe ManaCost
  effect    : Contextual (Magic ())

data TapCost = NoTapCost | TapCost  -- add later: UntapCost

data AbilityType = ActivatedAb | ManaAb | LoyaltyAb
  --deriving (Eq, Ord, Show, Read, Enum, Bounded)


-- Tipo extremamente complexo. Não devia estar nesse arquivo. Descobrir onde por isso.
StackItem : Type
StackItem = TargetList (ObjectRef TyStackItem -> PlayerRef -> Magic ())

-- StaticKeywordAbility
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
  -- deriving (Eq, Ord, Show, Read)

-- LayeredEffect

-- | A layered effect affects a set of objects and applies one or more
-- modifications to them. The order in which the effects are applied is
-- managed by layers [613]. By separating the affected objects from the
-- modifications, we can detect dependencies [613.7].
record LayeredEffect where
  affectedObjects     : Contextual (View (List SomeObjectRef))
  objectModifications : List ModifyObject

{- Haskell:
instance Show LayeredEffect where
  show _ = "(layered effect)"
-}

-- | Temporary layered effects are created by the resolution of instants,
-- sorceries and activated abilities.
record TemporaryLayeredEffect where
  temporaryTimestamp : Timestamp
  temporaryDuration  : Duration
  temporaryEffect    : LayeredEffect

{- Haskell:
instance Show TemporaryLayeredEffect where
  show _ = "(temporary layered effect)"
-}


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
  | RestrictAllowAttacks (List Attack -> Contextual (View Bool))
  | RestrictAllowBlocks (List Block -> Contextual (View Bool))

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
  --deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Duration with which a 'TemporaryLayeredEffect' can apply.
data Duration
  = Indefinitely
  | UntilEndOfTurn
  --deriving (Eq, Ord, Show, Read, Enum, Bounded)

ReplacementEffect : Type
ReplacementEffect = OneShotEffect -> Contextual (Maybe (Magic (List OneShotEffect)))

TriggeredAbilities : Type
TriggeredAbilities = List Event -> Contextual (View (List (Magic ())))
