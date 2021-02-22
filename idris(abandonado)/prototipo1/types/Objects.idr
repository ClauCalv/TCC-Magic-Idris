
-- https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/Types.hs

-- Cartas então são só receitas de criação de objetos? É uma abordagem interessante.
record Card where
  -- owner (and controller)
  instantiateCard :: PlayerRef -> Object

-- Como transformar isso em Vect n Card sem fazer Deck carregar n por aí?
Deck : Type
Deck = List Card
-- Usando DPair?
{- Idris:
Deck = (p ** Vect p Card)
-}


record Object where
  name       : Maybe Text
  colors     : Set Color -- Usar Data.AVL.Set
  types      : ObjectTypes
  owner      : PlayerRef
  controller : PlayerRef
  timestamp  : Timestamp

-- Conferir o que são os tipos Contextual e View:
  -- for creatures
  pt           : Maybe PT
  allowAttacks : List Attack -> Contextual (View Bool)
  allowBlocks  : List Block  -> Contextual (View Bool)

  -- for planeswalkers
  loyalty    : Maybe Int

  play                   : Maybe Activation
  alternativePlays       : List Activation
  staticKeywordAbilities : MultiSet StaticKeywordAbility -- Usar meu Multiset
  layeredEffects         : List LayeredEffect
  activatedAbilities     : List ActivatedAbility
  triggeredAbilities     : TriggeredAbilities
  replacementEffects     : List ReplacementEffect

  -- these fields are reset whenever this object changes zones
  counters               : MultiSet CounterType -- Usar meu Multiset
  temporaryEffects       : List TemporaryLayeredEffect

-- Eu tirei quase todas as instancias de show enquanto não vi necessidade, mas essa é especificamente interessante.
-- Eu não usaria Show no entanto para essa aplicação, mas fica aqui de lembrete pra quando eu escrever um 'nomeador' de objeto.
Show Object where
  show o =
    case name o of
      Nothing -> "(anonymous)"
      Just n  -> unpack n

-- Outro GADT? Eu juro que não entendi essa sintaxe do Haskell.
-- Era pra filtrar records que tenham esses campos?
-- Era pra criar um novo record dentro da declaração da função? Porque não fora e depois fazer X: Record -> A B
-- Como isso é usado? Será que era pra ser 3 records sob uma interface? Ou um tipo com esses 3 construtores?
{- Haskell:
data ObjectOfType : ObjectType -> Type where
  CardObject : { _cardObject :: Object
                } -> ObjectOfType TyCard
  Permanent  : { _permanentObject :: Object
                , _tapStatus       :: TapStatus
                , _damage          :: Int
                , _deathtouched    :: Bool
                , _attachedTo      :: Maybe SomeObjectRef
                , _attacking       :: Maybe EntityRef
                } -> ObjectOfType TyPermanent
  StackItem  : { _stackItemObject :: Object
                , _stackItem       :: StackItem
                } -> ObjectOfType TyStackItem
-}

-- Aqui o cara usou um monte de lenses que não entendo bem ainda.
{- Haskell:
-- Some hand-written lenses because fclabels doesn't support GADTs

cardObject :: ObjectOfType TyCard :-> Object
cardObject = lens _cardObject (\f rec -> rec { _cardObject = f (_cardObject rec) })

permanentObject :: ObjectOfType TyPermanent :-> Object
permanentObject = lens _permanentObject (\f rec -> rec { _permanentObject = f (_permanentObject rec) })

tapStatus :: ObjectOfType TyPermanent :-> TapStatus
tapStatus = lens _tapStatus (\f rec -> rec { _tapStatus = f (_tapStatus rec) })

damage :: ObjectOfType TyPermanent :-> Int
damage = lens _damage (\f rec -> rec { _damage = f (_damage rec) })

deathtouched :: ObjectOfType TyPermanent :-> Bool
deathtouched = lens _deathtouched (\f rec -> rec { _deathtouched = f (_deathtouched rec) })

attachedTo :: ObjectOfType TyPermanent :-> Maybe SomeObjectRef
attachedTo = lens _attachedTo (\f rec -> rec { _attachedTo = f (_attachedTo rec) })

attacking :: ObjectOfType TyPermanent :-> Maybe EntityRef
attacking = lens _attacking (\f rec -> rec { _attacking = f (_attacking rec) })

stackItemObject :: ObjectOfType TyStackItem :-> Object
stackItemObject = lens _stackItemObject (\f rec -> rec { _stackItemObject = f (_stackItemObject rec) })

stackItem :: ObjectOfType TyStackItem :-> StackItem
stackItem = lens _stackItem (\f rec -> rec { _stackItem = f (_stackItem rec) })
-}

-- https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/Predicates.hs

-- Predicates
-- todos os Data.Label.get foram removidos pq a sintaxe de Idris já constroi essas funções

hasName : Text -> Object -> Bool
hasName t o = t == name o

hasColor : Color -> Object -> Bool
hasColor c o = c `Set.member` colors o

isOwnedBy : PlayerRef -> Object -> Bool
isOwnedBy pr o = pr == owner o

isControlledBy : PlayerRef -> Object -> Bool
isControlledBy pr o = pr == controller o

-- | Checks whether the object's types are a superset of the given type set.
hasTypes : ObjectTypes -> Object -> Bool
hasTypes t o = t `isObjectTypesSubsetOf` types o

-- | Checks whether the object has one of the given type set.
hasOneOfTypes : List ObjectTypes -> Object -> Bool
hasOneOfTypes ts o = any (`hasTypes` o) ts

-- | Checks whether the object has at least one of the permanent card types.
hasPermanentType : Object -> Bool
hasPermanentType = or $ map hasTypes [artifactType, creatureType, enchantmentType, landType, planeswalkerType]

-- | Checks whether the object has a given static keyword ability.
hasStaticKeywordAbility : StaticKeywordAbility -> Object -> Bool
hasStaticKeywordAbility a o = a `elem` staticKeywordAbilities o

-- TODO entender isso
{-Haskell:
checkPermanent :: (Object -> Bool) -> ObjectRef TyPermanent -> View Bool
checkPermanent ok r = ok <$> asks (objectPart . object r)
-}
