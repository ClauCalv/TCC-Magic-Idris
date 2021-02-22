
-- !!!! Lembrar de avisar que o PGC já aprovou minha matrícula !!!!

module Mana

import Data.Vect

data Color = White | Blue | Black | Red | Green -- in this exact order: WUBRG
data ManaColor = ColoredMana Color | ColorlessMana

Mana : Type
Mana = (ManaColor, Nat)

allManaColors : Vect 6 ManaColor
allManaColors =
  [
    ColoredMana White,
    ColoredMana Blue,
    ColoredMana Black,
    ColoredMana Red,
    ColoredMana Green,
    ColorlessMana
  ]

-- Simplificado demais, mas por hora tá bom
record ManaPool where
    constructor MkManaPool
    white, blue, black, red, green, colorless : Nat

updateMana : ManaColor -> (Nat -> Nat) -> ManaPool -> ManaPool
updateMana color f = case color of
    ColoredMana White => record {white     $= f}
    ColoredMana Blue  => record {blue      $= f}
    ColoredMana Black => record {black     $= f}
    ColoredMana Red   => record {red       $= f}
    ColoredMana Green => record {green     $= f}
    ColorlessMana     => record {colorless $= f}

getMana : ManaColor -> ManaPool -> Nat
getMana color = case color of
    ColoredMana White => white
    ColoredMana Blue  => blue
    ColoredMana Black => black
    ColoredMana Red   => red
    ColoredMana Green => green
    ColorlessMana     => colorless

record ManaCost where
    constructor MkManaCost
    white, blue, black, red, green, colorless : Nat

updateCost : ManaColor -> (Nat -> Nat) -> ManaCost -> ManaCost
updateCost color f = case color of
    ColoredMana White => record {white     $= f}
    ColoredMana Blue  => record {blue      $= f}
    ColoredMana Black => record {black     $= f}
    ColoredMana Red   => record {red       $= f}
    ColoredMana Green => record {green     $= f}
    ColorlessMana     => record {colorless $= f}

getCost : ManaColor -> ManaCost -> Nat
getCost color = case color of
    ColoredMana White => white
    ColoredMana Blue  => blue
    ColoredMana Black => black
    ColoredMana Red   => red
    ColoredMana Green => green
    ColorlessMana     => colorless

------------------------------------------------------------

addMana : Mana -> ManaPool -> ManaPool
addMana (color, x) = updateMana color (+ x)

takeMana : Mana -> ManaPool -> Maybe ManaPool
takeMana (color, x) pool =
    let
        current = getMana color pool
    in
        if x > current then Nothing else Just (updateMana color (`minus` x) pool)

takeOneMana : ManaColor -> ManaPool -> Maybe ManaPool
takeOneMana color = takeMana (color, 1)

hasMana : ManaColor -> ManaPool -> Bool
hasMana a = isSucc . (getMana a)

-- "minus" nos naturais trata negativo como zero
payCost : Mana -> ManaCost -> (ManaCost, Mana)
payCost (color, x) cost =
    let
        current = getMana color pool
    in
        ((updateCost color (`minus` x) cost), (color, x `minus` current))

needsMana : ManaColor -> ManaCost -> Bool
needsMana a = isSucc . (getMana a)

isCostZero : ManaCost -> Bool
isCostZero = not . or . (map (\x => needsMana x cost)) allManaColors




{---
      Idéia: usar http://docs.idris-lang.org/en/latest/st/state.html
      junto com
}
