module WordGen2
( makeDictionary
, makeWord
, syllabifyWord
) where

import Prelude hiding (Word)
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Control.Monad
import Data.List
import Data.Ord

import PhonemeInventoryGen2
import PhonemeType2
import PhonotacticsGen2
import OtherData2

-- Generate words
-- Given number of syllables, consonant phonotactics, and vowel phonotactics, generate a list of words
makeDictionary :: Int -> [Phoneme] -> [[Phoneme]] -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int)) -> RVar [Word]
makeDictionary n vows sonhier settings = replicateM n (makeWord vows sonhier settings)

-- Generate a word given vowels, the consonant sonority hierarchy, and some settings
-- So far this only does vowel-nucleus syllables
-- Maybe should have a "completely random" version
makeWord :: [Phoneme] -> [[Phoneme]] -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int)) -> RVar Word
makeWord vows sonhier ((ns,xs), (ni,xi), (nt,xt), (ne,xe)) = do
  -- decide how many syllables in the word
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
  inter <- replicateM (s-1) $ join $ makeConsonantCluster <$> uniform nt xt <*> return (sonhier ++ reverse (init sonhier)) <*> return []
  --shuffle vowels and inside consonants together
  let inside = concat $ transpose [map (:[]) vowels, inter]

  return $ Word $ initial ++ concat inside ++ end

-- Generate a consonant cluster
-- Goes through sonority hierarchy taking consonants from each grouping
makeConsonantCluster :: Int -> [[Phoneme]] -> [Phoneme] -> RVar [Phoneme]
makeConsonantCluster 0 _ out = return out
makeConsonantCluster _ [] out = return out
makeConsonantCluster l sonhier out = do
  i <- uniform 0 1
  newCs <- sample i (head sonhier)
  makeConsonantCluster (l-i) (tail sonhier) (out ++ newCs)


-- Syllabification
-- Given a word and sonority hierarchy, syllabify the word
syllabifyWord :: [[Phoneme]] -> Word -> SyllWord
syllabifyWord sonhier (Word phones) = SyllWord sylls where
  groups = breakWord phones [] sonhier
  sylls = map (makeSyllable sonhier) groups

-- Given a group of phones, make a proper syllable structure
makeSyllable :: [[Phoneme]] -> [Phoneme] -> Syllable
makeSyllable sonhier phones = Syllable onset nucleus coda where
  nucleus = maximumBy (comparing (retrieveSon sonhier)) phones
  i = last $ elemIndices nucleus phones
  (onset, _)= splitAt i phones
  (_, coda)= splitAt (i+1) phones

-- Input the raw string of phones, output groups of phones that correspond to syllables
breakWord :: [Phoneme] -> [Phoneme] -> [[Phoneme]] -> [[Phoneme]]
breakWord [] syll sonhier = [syll]
breakWord phones [] sonhier = breakWord (init phones) [last phones] sonhier
breakWord phones syll sonhier
  -- start a new syllable for a vowel immediately after another vowel
  | retrieveSon sonhier (last phones) == length sonhier + 1 &&
    retrieveSon sonhier (head syll) == length sonhier + 1                  = breakWord phones [] sonhier ++ [syll]
  -- start new syllable when at local minimum (edge case)
  | length syll < 2 &&
    retrieveSon sonhier (last phones) > retrieveSon sonhier (head syll)    = breakWord (init phones) (last phones : syll) sonhier
  -- start new syllable when at local minimum
  | length syll >= 2 &&
    retrieveSon sonhier (last phones) > retrieveSon sonhier (head syll) &&
    retrieveSon sonhier (syll !! 1) >= retrieveSon sonhier (head syll)     = breakWord phones [] sonhier ++ [syll]
  -- otherwise add next phone to syllable
  | otherwise                                                              = breakWord (init phones) (last phones : syll) sonhier
