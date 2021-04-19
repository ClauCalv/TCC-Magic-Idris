module Magic.Types.References where

import Magic.Data.UniqueDict as UD

-- Ainda não entendi o uso de Some, então vai ficar no gelo por enquanto.

type IDList a = UniqueDict a

type PlayerRef = ID
type ObjectRef ty = (ZoneRef ty, ID)

-- type SomeObjectRef = (Some ZoneRef, ID)
-- type ActivatedAbilityRef = (SomeObjectRef, Int)

-- toSomeObjectRef :: ObjectRef ty -> SomeObjectRef
-- toSomeObjectRef (zoneRef, i) = (Some zoneRef, i)

-- Requer extensões:
-- 
data ZoneRef :: ObjectType -> Type where
  Library     :: PlayerRef -> ZoneRef TyCard
  Hand        :: PlayerRef -> ZoneRef TyCard
  Battlefield ::              ZoneRef TyPermanent
  Graveyard   :: PlayerRef -> ZoneRef TyCard
  Stack       ::              ZoneRef TyStackItem
  Exile       ::              ZoneRef TyCard
  Command     ::              ZoneRef TyCard

deriving instance Show (ZoneRef ty)
instance Show1 ZoneRef where show1 = show
deriving instance Eq (ZoneRef ty)

-- Não sei pra que eles usam isso ainda
-- instance TestEquality ZoneRef where
--   testEquality (Library p1)   (Library p2)   | p1 == p2 = Just Refl
--   testEquality (Hand p1)      (Hand p2)      | p1 == p2 = Just Refl
--   testEquality Battlefield Battlefield                  = Just Refl
--   testEquality (Graveyard p1) (Graveyard p2) | p1 == p2 = Just Refl
--   testEquality Stack Stack                              = Just Refl
--   testEquality Exile Exile                              = Just Refl
--   testEquality Command Command                          = Just Refl
--   testEquality _ _ = Nothing

data ObjectType = TyCard | TyPermanent | TyStackItem

-- type LastKnownObjectInfo = (SomeObjectRef, Object)

data EntityRef
  = PlayerRef PlayerRef
  | ObjectRef SomeObjectRef
  deriving (Eq, Show)

data TargetList a where
  Nil  :: a -> TargetList a
  Snoc :: TargetList (x -> y)
       -> EntityRef
       -> (EntityRef -> Maybe x)
       -> (y -> View Bool)
       -> (y -> a)
       -> TargetList a

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