module WordGen2
( makeDictionary
, makeMorpheme
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

-- Given consonant phonotactics, and vowel phonotactics, generate (one) root morpheme for a word
makeWord :: [Phoneme] -> [[Phoneme]] -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int)) -> RVar Word
makeWord vows sonhier ((ns,xs), (ni,xi), (nt,xt), (ne,xe)) = Word . (:[]) <$> makeMorpheme vows sonhier ((ns,xs), (ni,xi), (nt,xt), (ne,xe))

-- Generate a morpheme given vowels, the consonant sonority hierarchy, and some settings
-- So far this only does vowel-nucleus syllables
-- Maybe should have a "completely random" version
makeMorpheme :: [Phoneme] -> [[Phoneme]] -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int)) -> RVar Morpheme
makeMorpheme vows sonhier ((ns,xs), (ni,xi), (nt,xt), (ne,xe)) = do
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


-- Syllabification
-- Given a word and sonority hierarchy, syllabify the word
syllabifyWord :: [[Phoneme]] -> Word -> SyllWord
syllabifyWord sonhier (Word morphemes) = SyllWord sylls where
  -- Combine morphemes into one string of phonemes
  phonemes = concatMap getPhonemes morphemes
  groups = breakWord phonemes [] sonhier
  sylls = map (makeSyllable sonhier) groups

-- Given a group of phones, make a proper syllable structure
makeSyllable :: [[Phoneme]] -> [Phoneme] -> Syllable
makeSyllable sonhier phonemes = Syllable onset nucleus coda where
  nucleus = maximumBy (comparing (retrieveSon sonhier)) phonemes
  i = last $ elemIndices nucleus phonemes
  (onset, _)= splitAt i phonemes
  (_, coda)= splitAt (i+1) phonemes

-- Input the raw string of phones, output groups of phones that correspond to syllables
breakWord :: [Phoneme] -> [Phoneme] -> [[Phoneme]] -> [[Phoneme]]
breakWord [] syll sonhier = [syll]
breakWord phonemes [] sonhier = breakWord (init phonemes) [last phonemes] sonhier
breakWord phonemes syll sonhier
  -- start a new syllable for a vowel immediately after another vowel
  | retrieveSon sonhier (last phonemes) == length sonhier + 1 &&
    retrieveSon sonhier (head syll) == length sonhier + 1                  = breakWord phonemes [] sonhier ++ [syll]
  -- start new syllable when at local minimum (edge case)
  | length syll < 2 &&
    retrieveSon sonhier (last phonemes) > retrieveSon sonhier (head syll)    = breakWord (init phonemes) (last phonemes : syll) sonhier
  -- start new syllable when at local minimum
  | length syll >= 2 &&
    retrieveSon sonhier (last phonemes) > retrieveSon sonhier (head syll) &&
    retrieveSon sonhier (syll !! 1) >= retrieveSon sonhier (head syll)     = breakWord phonemes [] sonhier ++ [syll]
  -- otherwise add next phone to syllable
  | otherwise                                                              = breakWord (init phonemes) (last phonemes : syll) sonhier
