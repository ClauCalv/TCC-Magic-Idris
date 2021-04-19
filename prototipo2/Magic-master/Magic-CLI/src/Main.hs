module Main where

import Magic
import Magic.BasicLands
import Magic.CLI
import Magic.M13
import Magic.AvacynRestored

import Data.Monoid ((<>))

import System.IO

redDeck :: Deck
redDeck = replicate 18 mountain <> replicate 21 bloodArtist <> replicate 21 searingSpear

whiteDeck :: Deck
whiteDeck = replicate 24 plains
         -- <> replicate 4 angel'sMercy
         -- <> replicate 4 angelicBenediction
         -- <> replicate 8 attendedKnight
         -- <> replicate 4 avenSquire
         <> replicate 24 battleflightEagle
         -- <> replicate 4 captainOfTheWatch
         -- <> replicate 4 captain'sCall
         -- <> replicate 4 divineFavor

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  runGame [redDeck, whiteDeck]
