module Gen.Root
( makeRootDictionary
, makeRoot
) where

import Prelude hiding (Word)
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Control.Monad
import Data.List

import LoadStuff
import Data.Phoneme
import Data.Other
import Data.Inflection
import Data.Grammar
import Gen.Phonology
import Gen.Phonotactics

-- Generate root dictionary - atomic meanings
makeRootDictionary :: MeaningData -> [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> (Int, Int) -> RVar [((String, LexCat), Morpheme)]
makeRootDictionary mData vows ccs set = concat <$> sequence [n, v, a, p] where
  n = mapM (\i -> (,) <$> ((,) <$> return i <*> return Noun) <*> makeRoot vows ccs set) (inputNouns mData)
  v = mapM (\i -> (,) <$> ((,) <$> return i <*> return Verb) <*> makeRoot vows ccs set) (inputVerbs mData)
  a = mapM (\i -> (,) <$> ((,) <$> return i <*> return Adj) <*> makeRoot vows ccs set) (inputAdjs mData)
  p = mapM (\i -> (,) <$> ((,) <$> return i <*> return Adpo) <*> makeRoot vows ccs set) (inputAdpos mData)

-- Generate a morpheme given vowels, the consonant sonority hierarchy, and some settings
-- So far this only does vowel-nucleus syllables
-- Maybe should have a "completely random" version
makeRoot :: [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> (Int, Int) -> RVar Morpheme
makeRoot vows ccs (ns,xs) = do
  -- decide how many syllables in the morpheme
  s <- uniform ns xs
  root <- replicateM s (makeRootSyllable vows ccs)
  return $ Morpheme $ concat root

makeRootSyllable :: [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> RVar [Phoneme]
makeRootSyllable vows (onsets, codas) = do
  onset <- choice onsets
  vowel <- choice vows
  coda <- choice codas
  return $ onset ++ [vowel] ++ coda
