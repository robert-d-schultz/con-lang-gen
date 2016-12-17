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
import Gen.Phoneme
import Gen.Phonotactics

-- Generate root dictionary - atomic meanings
makeRootDictionary :: MeaningData -> [Phoneme] -> [[Phoneme]] -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int)) -> RVar [((String, LexCat), Morpheme)]
makeRootDictionary mData vows sonhier set = concat <$> sequence [n, v, a, p] where
  n = mapM (\i -> (,) <$> ((,) <$> return i <*> return Noun) <*> makeRoot vows sonhier set) (inputNouns mData)
  v = mapM (\i -> (,) <$> ((,) <$> return i <*> return Verb) <*> makeRoot vows sonhier set) (inputVerbs mData)
  a = mapM (\i -> (,) <$> ((,) <$> return i <*> return Adj) <*> makeRoot vows sonhier set) (inputAdjs mData)
  p = mapM (\i -> (,) <$> ((,) <$> return i <*> return Adpo) <*> makeRoot vows sonhier set) (inputAdpos mData)

-- Generate a morpheme given vowels, the consonant sonority hierarchy, and some settings
-- So far this only does vowel-nucleus syllables
-- Maybe should have a "completely random" version
makeRoot :: [Phoneme] -> [[Phoneme]] -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int)) -> RVar Morpheme
makeRoot vows sonhier ((ns,xs), (ni,xi), (nt,xt), (ne,xe)) = do
  -- decide how many syllables in the morpheme
  s <- uniform ns xs
  -- pick the initial consonant cluster
  i <- uniform ni xi
  initial <- makeConsonantCluster i (reverse sonhier) []
  -- pick the ending consonant cluster
  e <- uniform ne xe
  end <- makeConsonantCluster e sonhier []
  -- pick vowels
  vowels <- choices s vows
  -- pick inside consonant clusters
  inter <- replicateM (max (s-1) 0) $ join $ makeConsonantCluster <$> uniform nt xt <*> return (sonhier ++ reverse (init sonhier)) <*> return []
  --shuffle vowels and inside consonants together
  let inside = concat $ transpose [map (:[]) vowels, inter]

  return $ Morpheme $ initial ++ concat inside ++ end

-- Generate a consonant cluster
-- Goes through sonority hierarchy taking consonants from each grouping
makeConsonantCluster :: Int -> [[Phoneme]] -> [Phoneme] -> RVar [Phoneme]
makeConsonantCluster 0 _ out = return out
makeConsonantCluster _ [] out = return out
makeConsonantCluster l sonhier out = do
  i <- uniform 0 1
  newCs <- sample i (head sonhier)
  makeConsonantCluster (l-i) (tail sonhier) (out ++ newCs)
