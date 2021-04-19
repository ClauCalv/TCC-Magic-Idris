module Magic.Types.Objects.Object where

-- https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/Types.hs

data Card = Card
  -- owner (and controller)
  { instantiateCard :: PlayerRef -> Object
  }

type Deck = [Card]

data Object = Object
  { _name       :: Maybe Text
  , _colors     :: Set Color
  , _types      :: ObjectTypes
  , _owner      :: PlayerRef
  , _controller :: PlayerRef
  , _timestamp  :: Timestamp

  -- for creatures
  , _pt           :: Maybe PT
  , _allowAttacks :: [Attack] -> Contextual (View Bool)
  , _allowBlocks  :: [Block]  -> Contextual (View Bool)

  -- for planeswalkers
  , _loyalty    :: Maybe Int

  , _play                   :: Maybe Activation
  , _alternativePlays       :: [Activation]
  , _staticKeywordAbilities :: MultiSet StaticKeywordAbility
  , _layeredEffects         :: [LayeredEffect]
  , _activatedAbilities     :: [ActivatedAbility]
  , _triggeredAbilities     :: TriggeredAbilities
  , _replacementEffects     :: [ReplacementEffect]

  -- these fields are reset whenever this object changes zones
  , _counters               :: MultiSet CounterType
  , _temporaryEffects       :: [TemporaryLayeredEffect]
  }

instance Show Object where
  show o =
    case _name o of
      Nothing -> "(anonymous)"
      Just n  -> unpack n

data ObjectOfType :: ObjectType -> Type where
  CardObject :: { _cardObject :: Object
                } -> ObjectOfType TyCard
  Permanent  :: { _permanentObject :: Object
                , _tapStatus       :: TapStatus
                , _damage          :: Int
                , _deathtouched    :: Bool
                , _attachedTo      :: Maybe SomeObjectRef
                , _attacking       :: Maybe EntityRef
                } -> ObjectOfType TyPermanent
  StackItem  :: { _stackItemObject :: Object
                , _stackItem       :: StackItem
                } -> ObjectOfType TyStackItem

deriving instance Show (ObjectOfType ty)


-- Some hand-written lenses because fclabels doesn't support GADTs

-- cardObject :: ObjectOfType TyCard :-> Object
-- cardObject = lens _cardObject (\f rec -> rec { _cardObject = f (_cardObject rec) })

-- permanentObject :: ObjectOfType TyPermanent :-> Object
-- permanentObject = lens _permanentObject (\f rec -> rec { _permanentObject = f (_permanentObject rec) })

-- tapStatus :: ObjectOfType TyPermanent :-> TapStatus
-- tapStatus = lens _tapStatus (\f rec -> rec { _tapStatus = f (_tapStatus rec) })

-- damage :: ObjectOfType TyPermanent :-> Int
-- damage = lens _damage (\f rec -> rec { _damage = f (_damage rec) })

-- deathtouched :: ObjectOfType TyPermanent :-> Bool
-- deathtouched = lens _deathtouched (\f rec -> rec { _deathtouched = f (_deathtouched rec) })

-- attachedTo :: ObjectOfType TyPermanent :-> Maybe SomeObjectRef
-- attachedTo = lens _attachedTo (\f rec -> rec { _attachedTo = f (_attachedTo rec) })

-- attacking :: ObjectOfType TyPermanent :-> Maybe EntityRef
-- attacking = lens _attacking (\f rec -> rec { _attacking = f (_attacking rec) })

-- stackItemObject :: ObjectOfType TyStackItem :-> Object
-- stackItemObject = lens _stackItemObject (\f rec -> rec { _stackItemObject = f (_stackItemObject rec) })

-- stackItem :: ObjectOfType TyStackItem :-> StackItem
-- stackItem = lens _stackItem (\f rec -> rec { _stackItem = f (_stackItem rec) })

hasName :: Text -> Object -> Bool
hasName t o = return t == get name o

hasColor :: Color -> Object -> Bool
hasColor c o = c `Set.member` get colors o

isOwnedBy :: PlayerRef -> Object -> Bool
isOwnedBy pr o = pr == get owner o

isControlledBy :: PlayerRef -> Object -> Bool
isControlledBy pr o = pr == get controller o

-- | Checks whether the object's types are a superset of the given type set.
hasTypes :: ObjectTypes -> Object -> Bool
hasTypes t o = t `isObjectTypesSubsetOf` _types o

-- | Checks whether the object has one of the given type set.
hasOneOfTypes :: [ObjectTypes] -> Object -> Bool
hasOneOfTypes ts o = any (`hasTypes` o) ts

-- | Checks whether the object has at least one of the permanent card types.
hasPermanentType :: Object -> Bool
hasPermanentType = gor $ map hasTypes [artifactType, creatureType, enchantmentType, landType, planeswalkerType]

-- | Checks whether the object has a given static keyword ability.
hasStaticKeywordAbility:: StaticKeywordAbility -> Object -> Bool
hasStaticKeywordAbility a o = a `elem` get staticKeywordAbilities o


checkPermanent :: (Object -> Bool) -> ObjectRef TyPermanent -> View Bool
checkPermanent ok r = ok <$> asks (objectPart . object r)