
-- https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/Types.hs

-- OBJECT PROPERTIES

Timestamp : Type
Timestamp = Int

PT : Type
PT = (Int, Int)


data Color = White | Blue | Black | Red | Green

Eq Color where
  White == White  = True
  Blue  == Blue   = True
  Black == Black  = True
  Red   == Red    = True
  Green == Green  = True
  _     == _      = False

Enum Color where
  pred n = fromNat (Nat.pred (toNat n))
  succ n = fromNat (S (toNat n))
  toNat White = Z
  toNat Blue  = S Z
  toNat Black = S (S Z)
  toNat Red   = S (S (S Z))
  toNat Green = S (S (S (S Z)))
  fromNat Z           = White
  fromNat S Z         = Blue
  fromNat S (S Z)     = Black
  fromNat S (S (S Z)) = Red
  fromNat _           = Green

Ord Color where
  compare x y = compareEnum x y

MinBound Color where
  minBound = White

MaxBound Color where
  maxBound = Green


data TapStatus = Untapped | Tapped

Eq TapStatus where
  Untapped  == Untapped = True
  Tapped    == Tapped   = True
  _         == _        = False

Enum TapStatus where
  pred n = fromNat (Nat.pred (toNat n))
  succ n = fromNat (S (toNat n))
  toNat Untapped  = Z
  toNat Tapped    = S Z
  fromNat Z = Untapped
  fromNat _ = Tapped

Ord TapStatus where
  compare x y = compareEnum x y

MinBound TapStatus where
  minBound = Untapped

MaxBound TapStatus where
  maxBound = Tapped


data CounterType
  = Charge | Plus1Plus1 | Minus1Minus1 | Poison | Hatchling | Loyalty

Eq CounterType where
  Charge        == Charge       = True
  Plus1Plus1    == Plus1Plus1   = True
  Minus1Minus1  == Minus1Minus1 = True
  Poison        == Poison       = True
  Hatchling     == Hatchling    = True
  Loyalty       == Loyalty      = True
  _             == _            = False

Enum CounterType where
  pred n = fromNat (Nat.pred (toNat n))
  succ n = fromNat (S (toNat n))
  toNat Charge        = Z
  toNat Plus1Plus1    = S Z
  toNat Minus1Minus1  = S (S Z)
  toNat Poison        = S (S (S Z))
  toNat Hatchling     = S (S (S (S Z)))
  toNat Loyalty       = S (S (S (S (S Z))))
  fromNat Z               = Charge
  fromNat S Z             = Plus1Plus1
  fromNat S (S Z)         = Minus1Minus1
  fromNat S (S (S Z))     = Poison
  fromNat S (S (S (S Z))) = Hatchling
  fromNat _               = Loyalty

Ord CounterType where
  compare x y = compareEnum x y

MinBound CounterType where
  minBound = Charge

MaxBound CounterType where
  maxBound = Loyalty
