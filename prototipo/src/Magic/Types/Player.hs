module Magic.Types.Player where

--https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/Types.hs

data Player = Player
  { _life            :: Int
  , _manaPool        :: ManaPool
  , _prestack        :: [(LastKnownObjectInfo, Magic ())]  -- triggered abilities about to be put on the stack, together with their source
  , _library         :: IdList (ObjectOfType TyCard)
  , _hand            :: IdList (ObjectOfType TyCard)
  , _graveyard       :: IdList (ObjectOfType TyCard)
  , _maximumHandSize :: Maybe Int
  , _failedCardDraw  :: Bool  -- [704.5b]
  }