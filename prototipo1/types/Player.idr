
-- https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/Types.hs

record Player where
  life            : Int
  manaPool        : ManaPool
  prestack        : List (LastKnownObjectInfo, Magic ())  -- triggered abilities about to be put on the stack, together with their source
  library         : IdList (ObjectOfType TyCard)
  hand            : IdList (ObjectOfType TyCard)
  graveyard       : IdList (ObjectOfType TyCard)
  maximumHandSize : Maybe Int
  failedCardDraw  : Bool  -- [704.5b] (comprar de deck vazio)
