
-- https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/Types.hs

ManaPool : Type
ManaPool = MultiSet ManaEl

-- | Single element of a mana pool.
data ManaEl = ColorlessEl | ColorEl Color

Eq ManaEl where
  ColorlessEl == ColorlessEl  = True
  ColorEl c1  == ColorEl c2   = c1 == c2
  _           == _            = False

Ord ManaEl where
  compare ColorlessEl   ColorlessEl  = EQ
  compare (ColorEl c1)  (ColorEl c2) = compare c1 c2
  compare ColorlessEl   (ColorEl _)  = LT
  compare (ColorEl _)   ColorlessEl  = GT

ManaCost : Type
ManaCost = MultiSet ManaCostEl

-- | Single element of a mana cost.
data ManaCostEl = ManaElCost ManaEl | GenericCost
  -- Order is important: matches order of mana symbols on cards.

Eq ManaCostEl where
  GenericCost   == GenericCost    = True
  ManaElCost m1 == ManaElCost m2  = m1 == m2
  _             == _              = False

Ord ManaCostEl where
  compare GenericCost     GenericCost  = EQ
  compare (ManaElCost m1) (ManaElCost m2) = compare m1 m2
  compare GenericCost     (ManaElCost _)  = LT
  compare (ManaElCost _)  GenericCost  = GT



-- https://github.com/MedeaMelana/Magic/blob/master/Magic/src/Magic/Cost.hs

  white : Nat -> ManaCost
  white = mkColorCost White

  blue : Nat -> ManaCost
  blue = mkColorCost Blue

  black : Nat -> ManaCost
  black = mkColorCost Black

  red : Nat -> ManaCost
  red = mkColorCost Red

  green : Nat -> ManaCost
  green = mkColorCost Green

  mkColorCost : Color -> Nat -> ManaCost
  mkColorCost = mkManaCost . ManaElCost . ColorEl

  colorless : Nat -> ManaCost
  colorless = mkManaCost (ManaElCost (ColorlessEl))

  generic : Nat -> ManaCost
  generic = mkManaCost GenericCost

  mkManaCost : ManaCostEl -> Nat -> ManaCost
  mkManaCost x n = MultiSet.insertMany x n MultiSet.empty
    --| n >= 0     = MultiSet.insertMany x n MultiSet.empty
    --| otherwise  = error ("negative mana cost: " <> show n)

  cmc : ManaCost -> Nat
  cmc = MultiSet.size
