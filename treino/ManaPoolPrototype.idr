{-

Remainder:

Ctrl-Alt-A: Add definition
    Adds a skeleton definition for the name under the cursor
Ctrl-Alt-C: Case split
    Splits a definition into pattern-matching clauses for the name under the cursor
Ctrl-Alt-D: Documentation
    Displays documentation for the name under the cursor
Ctrl-Alt-L: Lift hole
    Lifts a hole to the top level as a new function declaration
Ctrl-Alt-M: Match
    Replaces a hole with a case expression that matches on an intermediate result
Ctrl-Alt-R: Reload
    Reloads and type-checks the current buffer
Ctrl-Alt-S: Search
    Searches for an expression that satisfies the type of the hole name under the cursor
Ctrl-Alt-T: Type-check name
    Displays the type of the name under the cursor

-}

import Data.Vect

data ManaColor = White | Blue | Black | Red | Green -- in this exact order: WUBRG

record ManaPool = {
  white : ColoredMana White Nat,
  blue : ColoredMana Blue Nat,
  black : ColoredMana Black Nat,
  red : ColoredMana Red Nat,
  green : ColoredMana Green Nat,
  colorless : ColorlessMana Nat
}

record ManaCost = {
  white : Nat,
  blue : Nat,
  black : Nat,
  red : Nat,
  green : Nat,
  colorless : Nat
}

allColors : Vect 5 ManaColor
allColors =  [ White, Blue, Black, Red, Green ]

data Mana = ColoredMana ManaColor Nat | ColorlessMana Nat
data ManaCostElement = ColoredCost ManaColor | BicolorCost ManaColor ManaColor | ColorlessCost | ColoredOr2Incolor ManaColor

--Checar https://github.com/spydr073/AA-Tree/blob/master/src/Data/AA/Set/MultiSet.idr
--import Data.AA.Set.Multiset

{-
 MANEIRA 1
-}
-- O que eu queria fazer mas não consigo nenhuma maneira de fazer isso:
-- deriving (Enum, Bounded) do Haskell
-- https://stackoverflow.com/a/45725130/13843556
-- Ambas as interfaces Enum e MaxBound existem no prelude mas não funcionam como eu acho que deveriam
interface BoundedEnum a where
    enumSize : Nat
    getByIndex : Nat -> a
    getIndexOf : a -> Nat

BoundedEnum Mana where
    enumSize = 6
    getByIndex x =
      let i = the Int (cast x) in
        case i `mod` 6 of
          0 => ColoredMana White
          1 => ColoredMana Blue
          2 => ColoredMana Black
          3 => ColoredMana Red
          4 => ColoredMana Green
          5 => ColorlessMana
    getIndexOf x =
      case x of
          ColoredMana White => cast 0
          ColoredMana Blue => cast 1
          ColoredMana Black => cast 2
          ColoredMana Red => cast 3
          ColoredMana Green => cast 4
          ColorlessMana => cast 5

BoundedEnum ManaCostElement where
    ...


FixedMultiSet : (a : Type) -> BoundedEnum b => {(b : a)} -> Type
FixedMultiSet type = Vect enumSize (type, Nat)

FixedMultiSet : (a : Type) -> Type
FixedMultiSet (BoundedEnum type => type) = Vect enumSize (type, Nat)

ManaPool : Type
ManaPool = FixedMultiSet Mana

emptyManaPool : ManaPool
emptyManaPool = map (\x-> (getByIndex x, 0)) [0 .. enumSize]
emptyManaPool = zip allColors (repeat 0)

ManaCost : Type
ManaPool = FixedMultiSet ManaCostElement

createCost : List (ManaCostElement, Nat) -> ManaCost
createCost costs = map (\x -> let el = getByIndex x in (el, cost el)) [0 .. enumSize]
    where
          cost el = foldl (\(x, c), acc -> if (x == el) then acc + c else acc) 0 costs

-- foldlM : Foldable t => Monad m => (funcM : a -> b -> m a) -> (init : a) -> (input : t b) -> m a
-- t : Vect n
-- m : Maybe
-- a : Vect m (Mana, Nat)
-- b : (ManaCostElement, Nat)
payCosts : ManaPool -> ManaCosts -> Maybe ManaPool
payCosts pool costs = foldlM payCostElement pool costs

-- funcM : a -> b -> m a
-- m : Maybe
-- a : Vect m (Mana, Nat)
-- b : (ManaCostElement, Nat)
payCostElement : ManaPool -> (ManaCostElement, Nat) -> Maybe ManaPool
payCostElement pool (cost, amount) = case cost of

        BicolorCost c1 c2 ->
            let
                amount1 = findAmount (ColoredMana c1) pool
                amount2 = findAmount (ColoredMana c2) pool
                finalPool = payAmounts amount amount1 amount2
            in
                case finalPool of
                    Just (newAmount1, newAmount2) ->
                        map (\(m, x) -> if m == ColoredMana c1 then newAmount1 else if m == ColoredMana c2 then newAmount2 else x) pool
                    Nothing -> Nothing

        ColoredCost c1 ->
            let
                amount1 = findAmount (ColoredMana c1) pool
                finalPool = payAmount amount amount1
            in
                case finalPool of
                    Just newAmount1 -> map (\(m, x) -> if m == ColoredMana c1 then newAmount1 else x) pool
                    Nothing -> Nothing

        ColorlessCost ->
            let
                amount1 = findAmount (Colorless c1) pool
                finalPool = payAmount amount amount1
            in
                case finalPool of
                    Just newAmount1 -> map (\(m, x) -> if m ==ColoredMana c1 then newAmount1 else x) pool
                    Nothing -> Nothing
    where
        findAmount c = snd . head . filter (\(x,y) -> x == c)

        payAmount  Z     n     = Just n
        payAmount (S k) (S k') = payAmount k k'
        payAmount (S k)  Z     = Nothing

        payAmounts (S k)  n      m      = Just (m, n)
        payAmounts (S k) (S k')  m      = payAmounts k k' n
        payAmounts (S k)  Z     (S k'') = payAmounts k Z  k''
        payAmounts (S k)  Z      Z      = Nothing
