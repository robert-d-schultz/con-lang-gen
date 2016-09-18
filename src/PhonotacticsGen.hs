module PhonotacticsGen
( splitC
, splitV
) where

import Prelude
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import PhonemeInventoryGen
import PhonemeType
import Data.Tuple.Sequence
import qualified Data.Set as Set

-- Generates phonotactic rules to be used by the word generator, inflection generator

-- By importance:
-- * Allowed syllable types
-- * Onset/Coda consonants
-- * Onset/Nucleus/Coda vowels
--allowedType :: [SyllType]

-- Split consonants into two groups, onset and coda
splitC :: [MaybeConsonant] -> RVar ([MaybeConsonant], [MaybeConsonant])
splitC cons = do
  let consB =  cons ++ [Blank]
  foo <- uniform 0 (length consB)
  let (onset, rest) = splitAt foo consB
  bar <- uniform 0 (length onset)
  foobar <- sample bar onset
  let coda = rest ++ foobar
  return (onset, coda)

-- Split vowels into X groups, onset, nucleus, coda, and alone
-- Check the result of splitC for each of these

-- The union of all 4 has to be equal to the vowel invetory
-- nucleus = [] if either onsetC or codaC is only []
-- onset = [] if onsetC doesn't have [] in it
-- coda = [] if codaC doesn't have [] in it
-- alone = [] if either codaC or onsetC doesnt not have [] in it (need both to have it)
-- I think using sets would be the best course of action...


splitV :: ([MaybeConsonant], [MaybeConsonant]) -> [Vowel] -> RVar ([Vowel], [Vowel], [Vowel], [Vowel])
splitV (onsetC, codaC) vows = output where
  nucleus
    | null onsetC || null codaC                       = return []
    | otherwise                                       = getNucleus vows

  onset
    | Blank `notElem` onsetC                          = return []
    | otherwise                                       = getOnset vows

  coda
    | Blank `notElem` codaC                           = return []
    | otherwise                                       = getCoda vows

  alone
    | Blank `notElem` codaC || Blank `notElem` onsetC = return []
    | otherwise                                       = getAlone vows

  output = check vows (nucleus,onset,coda,alone)

check :: [Vowel] -> (RVar [Vowel],RVar [Vowel],RVar [Vowel],RVar [Vowel]) -> RVar ([Vowel],[Vowel],[Vowel],[Vowel])
check vows (nucleus,onset,coda,alone) = do
  n <- nucleus
  o <- onset
  c <- coda
  a <- alone
  let sup = Set.fromList vows
  let sub = Set.unions [Set.fromList n, Set.fromList o, Set.fromList c, Set.fromList a]
  let dif = Set.toList (Set.difference sub sup)

  return (nEmpty n dif,oEmpty o dif,cEmpty c dif,aEmpty a dif)

nEmpty [] _  = []
nEmpty nb dif = nb ++ dif

oEmpty [] _  = []
oEmpty ob dif = ob ++ dif

cEmpty [] _ = []
cEmpty cb dif = cb ++ dif

aEmpty [] _ = []
aEmpty ab dif = ab ++ dif

getOnset :: [Vowel] -> RVar [Vowel]
getOnset vows = do
  n1 <- uniform 0 (length vows)
  sample n1 vows

getCoda :: [Vowel] -> RVar [Vowel]
getCoda vows = do
  n2 <- uniform 0 (length vows)
  sample n2 vows

getNucleus :: [Vowel] -> RVar [Vowel]
getNucleus vows = do
  n3 <- uniform 0 (length vows)
  sample n3 vows

getAlone :: [Vowel] -> RVar [Vowel]
getAlone vows = do
  n4 <- uniform 0 (length vows)
  sample n4 vows


-- * Allowed/disallowed consonant clusters

-- * Syllable patterns? (CVC.VC.CV) etc... did this before
