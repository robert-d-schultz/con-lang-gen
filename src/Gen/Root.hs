{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Gen.Root
( makeRootDictionary
, makeRoot
) where

import ClassyPrelude hiding (Word)
import Control.Monad as R (replicateM)
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)

import LoadStuff
import Data.Phoneme
import Data.Other
import Data.Inflection
import Data.Grammar
import Gen.Phonology
import Gen.Phonotactics

-- Generate root dictionary - atomic meanings
makeRootDictionary :: MeaningData -> [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> (Int, Int) -> RVar [((Text, LexCat), Morpheme)]
makeRootDictionary mData vows ccs set = concat <$> sequence [n, v, a, p] where
  n = mapM (\i -> (,) <$> ((,) <$> return i <*> return Noun) <*> makeRoot vows ccs set) (inputNouns mData)
  v = mapM (\i -> (,) <$> ((,) <$> return i <*> return Verb) <*> makeRoot vows ccs set) (inputVerbs mData)
  a = mapM (\i -> (,) <$> ((,) <$> return i <*> return Adj) <*> makeRoot vows ccs set) (inputAdjs mData)
  p = mapM (\i -> (,) <$> ((,) <$> return i <*> return Adpo) <*> makeRoot vows ccs set) (inputAdpos mData)

-- Generate a morpheme given vowels, consonant clusters, and some settings
makeRoot :: [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> (Int, Int) -> RVar Morpheme
makeRoot vows ccs (ns,xs) = do
  -- decide how many syllables in the morpheme
  s <- uniform ns xs
  root <- R.replicateM s (makeRootSyllable vows ccs)
  return $ Morpheme $ concat root

makeRootSyllable :: [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> RVar [Phoneme]
makeRootSyllable vows (onsets, codas) = do
  onset <- choice onsets
  vowel <- choice vows
  coda <- choice codas
  return $ onset ++ [vowel] ++ coda

-- Gen.Meaning?
-- special generators
makeColorSystem :: RVar [Text]
makeColorSystem = do
  ncolors <- uniform 0 11 :: RVar Int
  case ncolors of
    0 -> return []
    1 -> sample 1 ["white", "black"]
    2 -> return ["white", "black"]
    3 -> return ["white", "black", "red"]
    4 -> (++) ["white", "black", "red"] <$> sample 1 ["yellow", "green"]
    5 -> return ["white", "black", "red", "yellow", "green"]
    6 -> return ["white", "black", "red", "yellow", "green", "blue"]
    7 -> return ["white", "black", "red", "yellow", "green", "blue", "brown"]
    8 -> (++) ["white", "black", "red", "yellow", "green", "blue", "brown"] <$> sample 1 ["purple", "pink", "orange", "gray"]
    9 -> (++) ["white", "black", "red", "yellow", "green", "blue", "brown"] <$> sample 2 ["purple", "pink", "orange", "gray"]
    10 -> (++) ["white", "black", "red", "yellow", "green", "blue", "brown"] <$> sample 3 ["purple", "pink", "orange", "gray"]
    11 -> return ["white", "black", "red", "yellow", "green", "blue", "brown", "pueple", "pink", "orange", "gray"]

makeNumberSystem :: RVar [Text]
makeNumberSystem = do
  base <- choice [2, 4, 5, 6, 8, 10, 12, 15, 16, 20, 40, 60]
  return $ map tshow ([0..base] ++ ((^) <$> [base] <*> [2..6]))
