module Magic.Types.World where

import Magic.Types.TurnStructure
import Magic.Types.References

-- https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/Types.hs

data World = World
  { _players       :: IdList Player
  , _activePlayer  :: PlayerRef
  , _activeStep    :: Step
  , _time          :: Timestamp
  , _turnStructure :: [(PlayerRef, [Step])]
  , _exile         :: IdList (ObjectOfType TyCard)
  , _battlefield   :: IdList (ObjectOfType TyPermanent)
  , _stack         :: IdList (ObjectOfType TyStackItem)
  , _command       :: IdList (ObjectOfType TyCard)
  , _turnHistory   :: [Event]
  }