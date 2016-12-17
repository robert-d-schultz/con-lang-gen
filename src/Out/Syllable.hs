module Out.Syllable
( syllabifyWord
, syllabifyMorpheme
) where

import Prelude hiding (Word)
import Data.List
import Data.Ord

import Data.Phoneme
import Gen.Phonotactics
import Data.Other

-- Syllabification
-- Given a word and sonority hierarchy, syllabify the word
syllabifyWord :: [[Phoneme]] -> Word -> SyllWord
syllabifyWord sonhier (Word morphemes) = SyllWord sylls where
  -- Combine morphemes into one string of phonemes
  phonemes = concatMap getPhonemes morphemes
  groups = breakWord phonemes [] sonhier
  sylls = map (makeSyllable sonhier) groups

syllabifyMorpheme :: [[Phoneme]] -> Morpheme -> SyllWord
syllabifyMorpheme sonhier morpheme = SyllWord sylls where
  phonemes = getPhonemes morpheme
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
    retrieveSon sonhier (head syll) == length sonhier + 1                    = breakWord phonemes [] sonhier ++ [syll]
  -- start new syllable when at local minimum (edge case)
  | length syll < 2 &&
    retrieveSon sonhier (last phonemes) > retrieveSon sonhier (head syll)    = breakWord (init phonemes) (last phonemes : syll) sonhier
  -- start new syllable when at local minimum
  | length syll >= 2 &&
    retrieveSon sonhier (last phonemes) > retrieveSon sonhier (head syll) &&
    retrieveSon sonhier (syll !! 1) >= retrieveSon sonhier (head syll)       = breakWord phonemes [] sonhier ++ [syll]
  -- otherwise add next phone to syllable
  | otherwise                                                                = breakWord (init phonemes) (last phonemes : syll) sonhier
