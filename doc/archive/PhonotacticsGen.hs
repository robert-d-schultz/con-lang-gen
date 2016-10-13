module PhonotacticsGen
( makeConPhonotactics
, makeVowPhonotactics
) where

-- Import
import Prelude
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Data.Tuple.Sequence
import qualified Data.Set as Set

import PhonemeInventoryGen
import PhonemeType

-- Generates phonotactic rules to be used by the word generator, inflection generator

-- By importance:
-- * Allowed syllable types
-- * Onset/Coda consonants
-- * Onset/Nucleus/Coda vowels
--allowedType :: [SyllType]

-- Split consonants into two groups, onset and coda
makeConPhonotactics :: [MaybeConsonant] -> RVar ([MaybeConsonant], [MaybeConsonant])
makeConPhonotactics cons = do
  let consB =  cons ++ [Blank]
  n1 <- uniform 0 (length consB)
  let (onset, codaOnly) = splitAt n1 consB
  t <- onsetCoda n1 onset
  let coda = codaOnly ++ t
  return (cleanTactics onset, cleanTactics coda)

onsetCoda :: Int -> [MaybeConsonant] -> RVar [MaybeConsonant]
onsetCoda 0 onset = return []
onsetCoda n onset = do
  n2 <- uniform 0 n
  sample n2 onset

cleanTactics :: [MaybeConsonant] -> [MaybeConsonant]
cleanTactics cons
  | null cons = [Blank]
  | otherwise = cons

-- Split vowels into X groups, onset, nucleus, coda, and alone
-- Check the result of splitC for each of these

-- The union of all 4 has to be equal to the vowel invetory
-- nucleus = [] if either onsetC or codaC is only []
-- onset = [] if onsetC doesn't have [] in it
-- coda = [] if codaC doesn't have [] in it
-- alone = [] if either codaC or onsetC doesnt not have [] in it (need both to have it)
-- I think using sets would be the best course of action...


makeVowPhonotactics :: ([MaybeConsonant], [MaybeConsonant]) -> [Vowel] -> RVar ([Vowel], [Vowel], [Vowel], [Vowel])
makeVowPhonotactics (onsetC, codaC) vows = output where
  nucleus
    | onsetC == [Blank] || codaC == [Blank] = return []
    | otherwise                  = do
      n3 <- uniform 1 (length vows)
      sample n3 vows

  onset
    | Blank `elem` onsetC = do
      n1 <- uniform 1 (length vows)
      sample n1 vows
    | otherwise  = return []

  coda
    | Blank `elem` codaC = do
      n2 <- uniform 1 (length vows)
      sample n2 vows
    | otherwise = return []

  alone
    | (Blank `elem` codaC) && (Blank `elem` onsetC) = do
      n4 <- uniform 1 (length vows)
      sample n4 vows
    | otherwise = return []

  output = do
    n <- nucleus
    o <- onset
    c <- coda
    a <- alone
    let sup = Set.fromList vows
    let sub = Set.unions [Set.fromList n, Set.fromList o, Set.fromList c, Set.fromList a]
    let dif = Set.toList (Set.difference sup sub)

    return (nEmpty n dif,oEmpty o dif,cEmpty c dif,aEmpty a dif)

  nEmpty [] _  = []
  nEmpty nb dif = nb ++ dif

  oEmpty [] _  = []
  oEmpty ob dif = ob ++ dif

  cEmpty [] _ = []
  cEmpty cb dif = cb ++ dif

  aEmpty [] _ = []
  aEmpty ab dif = ab ++ dif


-- * Allowed/disallowed consonant clusters

-- * Syllable patterns? (CVC.VC.CV) etc... did this before
