module WordGen2
( makeDictionary
, makeWord
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

-- Generates words

-- Given number of syllables, consonant phonotactics, and vowel phonotactics, generate a list of words
makeDictionary :: Int -> [Phone] -> [[Phone]] -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int)) -> RVar [Word]
makeDictionary n vows sonhier settings = replicateM n (makeWord vows sonhier settings)

-- Generate a word given vowels, the consonant sonority hierarchy, and some settings
makeWord :: [Phone] -> [[Phone]] -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int)) -> RVar Word
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
  t <- uniform nt (min xt s-1)
  inter <- makeConsonantCluster t (sonhier ++ reverse (init sonhier)) []
  --shuffle vowels and inside consonants together
  let inside = concat $ transpose [vowels,inter]

  return $ Word $ initial ++ inside ++ end

-- Generate a consonant cluster
makeConsonantCluster :: Int -> [[Phone]] -> [Phone] -> RVar [Phone]
makeConsonantCluster 0 _ out = return out
makeConsonantCluster _ [] out = return out
makeConsonantCluster l sonhier out = do
  i <- uniform 0 1
  newCs <- sample i (head sonhier)
  makeConsonantCluster (l-i) (tail sonhier) (out ++ newCs)

syllabifyWord :: Word -> [[Phone]] -> SyllWord
syllabifyWord (Word phones) sonhier = map SyllWord sylls where
  sylls = breakWord phones [] sonhier
  --local maximums = nucleus
  --add falling sonority to the onsets
  --add falling sonority to coda

makeSyllable :: [Phone] -> [[Phone]] -> Syllable
makeSyllable phones sonhier = Syllable onset nucleus coda where
  nucleus = maximumBy (comparing (retrieveSon sonhier)) phones
  map (retriveSon sonhier) phones



breakWord :: [Phone] -> [Phone] -> [[Phone]] -> [[Phone]]
breakWord [] _ sonhier = []
breakWord phones [] sonhier = breakWord (init phones) [last phones] sonhier
breakWord phones syll sonhier
  | retrieveSon (last phones) sonhier == length sonhier + 1 &&
    retrieveSon (head syll) sonhier == length sonhier + 1                = breakWord phones [] sonhier ++ [syll]
  | retrieveSon (last phones) sonhier <= retrieveSon (head syll) sonhier = breakWord (init phones) (last phones : syll) sonhier
  | otherwise                                                            = breakWord phones [] sonhier ++ [syll]
