module Magic.Types.Mana where


type ManaPool = MultiSet ManaEl

-- | Single element of a mana pool.
data ManaEl = ColorlessEl | ColorEl Color
  deriving (Eq, Ord, Show, Read)

type ManaCost = MultiSet ManaCostEl

-- | Single element of a mana cost.
data ManaCostEl = ManaElCost ManaEl | GenericCost
  -- Order is important: matches order of mana symbols on cards.
  deriving (Eq, Ord, Show, Read)

white :: Int -> ManaCost
white = mkColorCost White

blue :: Int -> ManaCost
blue = mkColorCost Blue

black :: Int -> ManaCost
black = mkColorCost Black

red :: Int -> ManaCost
red = mkColorCost Red

green :: Int -> ManaCost
green = mkColorCost Green

mkColorCost :: Color -> Int -> ManaCost
mkColorCost = mkManaCost . ManaElCost . ColorEl

colorless :: Int -> ManaCost
colorless = mkManaCost (ManaElCost (ColorlessEl))

generic :: Int -> ManaCost
generic = mkManaCost GenericCost

mkManaCost :: ManaCostEl -> Int -> ManaCost
mkManaCost x n 
  | n >= 0     = MultiSet.insertMany x n MultiSet.empty
  | otherwise  = error ("negative mana cost: " <> show n)

cmc :: ManaCost -> Int
cmc = MultiSet.size