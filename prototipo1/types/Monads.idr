
-- https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/Types.hs

-- MONADS ViewT AND View

--TODO

{- Haskell:
newtype ViewT m a = ViewT { runViewT :: ReaderT World m a }
  deriving (Functor, Applicative, Monad, MonadReader World, MonadTrans)

instance (Semigroup a, Monad m) => Semigroup (ViewT m a) where
  ViewT x <> ViewT y = ViewT (liftM2 (<>) x y)

instance (Monoid a, Monad m) => Monoid (ViewT m a) where
  mempty = return mempty

instance (Monad m, Boolean a) => Boolean (ViewT m a) where
  true  = return true
  false = return false
  notB  = liftM  notB
  (&&*) = liftM2 (&&*)
  (||*) = liftM2 (||*)

type View = ViewT Identity

runView :: View a -> World -> a
runView v w = runIdentity (runReaderT (runViewT v) w)

class MonadView m where
  view :: View a -> m a

instance Monad m => MonadView (ViewT m) where
  view (ViewT (ReaderT f)) = liftM (runIdentity . f) ask



-- MONADIC INTERACTION WITH PLAYERS


data Interact a where
  Debug       :: Text -> Interact ()
  LogEvents   :: EventSource -> [Event] -> World -> Interact ()
  AskQuestion :: PlayerRef -> World -> Question a -> Interact a

data EventSource
  = TurnBasedActions
    -- ^ Events caused by turn-based actions
  | StateBasedActions
    -- ^ Events caused by state-based actions
  | StackTrigger LastKnownObjectInfo
    -- ^ Events caused by putting a trigger on the stack
  | ResolutionOf (ObjectRef TyStackItem)
    -- ^ Events caused by the resolution of a spell or ability
  | PriorityActionExecution PriorityAction
    -- ^ Events caused by casting a spell or activating an ability
  deriving Show

data Choice =
    ChoiceYesNo Bool
  | ChoiceColor Color
  | ChoiceCard SomeObjectRef
  | ChoiceText Text

data Question a where
  AskKeepHand              :: Question Bool
  AskPriorityAction        :: [PriorityAction] -> Question (Maybe PriorityAction)
  AskManaAbility           :: ManaCost -> [PayManaAction] -> Question PayManaAction
  AskTarget                :: [EntityRef] -> Question EntityRef
  AskMaybeTarget           :: [EntityRef] -> Question (Maybe EntityRef)
  --AskPickReplacementEffect :: [(ReplacementEffect, Magic [OneShotEffect])] -> Question (Pick (ReplacementEffect, Magic [OneShotEffect]))
  AskPickTrigger           :: [LastKnownObjectInfo] -> Question Int
  AskAttackers             :: [ObjectRef TyPermanent] -> [EntityRef] -> Question [Attack]
  AskSearch                :: ZoneRef ty -> [Id] -> Question (Maybe Id)
  AskChoice                :: Maybe Text -> [(Choice, a)] -> Question a

type Pick a = (a, [a])

class Monad m => MonadInteract m where
  interact :: Program Interact a -> m a



-- MONAD Magic


newtype Magic a = Magic { runMagic :: ViewT (ProgramT ExecuteEffects (Program Interact)) a }
  deriving (Functor, Applicative, Monad)

data ExecuteEffects a where
  ExecuteEffects :: [OneShotEffect] -> ExecuteEffects [Event]
  Tick           :: ExecuteEffects Timestamp

instance MonadView Magic where
  view = Magic . view

instance MonadInteract Magic where
  interact = Magic . lift . lift

instance Semigroup a => Semigroup (Magic a) where
  (<>) = liftM2 (<>)

instance Monoid a => Monoid (Magic a) where
  mempty  = return mempty


mkLabels [''World, ''Player, ''Object]
-}
