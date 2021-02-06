

-- https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/Types.hs


PlayerRef : Type
PlayerRef = Id

ObjectRef : Type -> Type
ObjectRef ty = (ZoneRef ty, Id)


-- Não entendi ainda o motivo da monad 'Some' (https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/Some.hs) então não mexi ainda
{- Haskell:
type SomeObjectRef = (Some ZoneRef, Id)
type ActivatedAbilityRef = (SomeObjectRef, Int)
toSomeObjectRef :: ObjectRef ty -> SomeObjectRef
toSomeObjectRef (zoneRef, i) = (Some zoneRef, i)
-}

-- Essa parte estava escrita em GADT, então nenhum trabalho a ser feito
data ZoneRef : ObjectType -> Type where
  Library     : PlayerRef -> ZoneRef TyCard
  Hand        : PlayerRef -> ZoneRef TyCard
  Battlefield :              ZoneRef TyPermanent -- Só permanentes podem estar em campo
  Graveyard   : PlayerRef -> ZoneRef TyCard
  Stack       :              ZoneRef TyStackItem -- Só itens de pilha podem estar na pilha (magias não-terreno ou efeitos)
  Exile       :              ZoneRef TyCard
  Command     :              ZoneRef TyCard

-- Saudades de 'deriving'
Eq ZoneRef where
  (Library p1)    == (Library p2)   = p1 == p2
  (Hand p1)       == (Hand p2)      = p1 == p2
  Battlefield     == Battlefield    = True
  (Graveyard p1)  == (Graveyard p2) = p1 == p2
  Stack           == Stack          = True
  Exile           == Exile          = True
  Command         == Command        = True
  _               == _              = False


data ObjectType = TyCard | TyPermanent | TyStackItem

-- Mesma coisa de 'Some'
{- Haskell:
type LastKnownObjectInfo = (SomeObjectRef, Object)
-}



-- TARGETS

data EntityRef
  = PlayerRef PlayerRef
  | ObjectRef SomeObjectRef
  --deriving (Eq, Show)

Eq EntityRef where
  PlayerRef p1 == PlayerRef p2 = p1 == p2
  ObjectRef o1 == ObjectRef o2 = o1 == o2
  _            ==              = False

--Não sei ainda como traduzir isso
{- Haskell:
data TargetList a where
  Nil  :: a -> TargetList a
  Snoc :: TargetList (x -> y)
       -> EntityRef
       -> (EntityRef -> Maybe x)
       -> (y -> View Bool)
       -> (y -> a)
       -> TargetList a
-}

--Só faz sentido traduzir isso quando souber o que fazer em cima.
{- Haskell:
instance Functor TargetList where
  fmap f (Nil x)        = Nil (f x)
  fmap f (Snoc ts e cast test g) = Snoc ts e cast test (f . g)

instance Applicative TargetList where
  pure = Nil
  xs <*> Nil b     = fmap ($ b) xs
  xs <*> Snoc ys e cast test g =
      Snoc (f <$> xs <*> ys) e cast test' g'
    where
      f ab xy x    = (ab, xy x)
      test' (_, y) = test y
      g' (ab, y)   = ab (g y)

instance Semigroup a => Semigroup (TargetList a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (TargetList a) where
  mempty  = pure mempty

instance Show (TargetList a) where
  show _ = "<target list>"
-}
