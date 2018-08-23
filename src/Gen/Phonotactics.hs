{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
module Gen.Phonotactics
( makeSonHier
, retrieveSon
, makeOnsets
, makeNuclei
, makeCodas
) where

import ClassyPrelude

import Data.List (elemIndex)

import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)

import Data.Phoneme

-- Plan:
-- Make sonority hierarchies be rule-based
-- Rules will be procedurally generated, and recorded per-language
-- This will allow the hierarchy to be reconstructed when new phonemes are introduced

-- Question:
-- What is the relation between the sonority hierarchy and valid consonant clusters?
-- Are they the exact same ie. all clusters that the can come out of the hierarchy are valid?
-- Well, no: /s/ can violate it in some languages (and not in others)
-- Also, valid English CC's are (seemingly) subset of all possible ones (/dv/, /vl/, /tl/ are "bad" and from loans)

-- Constraints/Rules:
-- Minimum Sonority Distance
-- Like if the SH is glide-liquid-nasal-fricative-stop
-- and the MSD is 2, you can't have stop-fricative onsets
-- Some languages allow 0 (stop-stop is valid, for instance)

-- Identical Place of Articulation
-- Apparently having the same POA in consecutive consonants (in a syllable onset) is bad?


-- Aside:
-- The available consonant clusters can effect sound changes
-- Like if a consonant changes in a particular word, and it results in a bad CC
-- then the consonant can instead be deleted, or a vowel can instead be inserted
-- On the other hand, the sound changes can effect the available consonant clusters
-- Like in the above example, the resultant "bad" CC can be added in as an available one

-- In either case there needs to be a function that runs over a recently changed lexicon and looks for these bad CCs
-- Then it can either decide to fix them or add them


-- Generate valid onsets
makeOnsets :: [[Phoneme]] -> (Int, Int) -> RVar [[Phoneme]]
makeOnsets sonHier (msd, maxOnsetC) = do
  n <- uniform 5 10
  replicateM n $ makeConsonantCluster (msd, maxOnsetC) sonHier []

-- Pick valid nuclei, its just vowels/diphthongs for right now
makeNuclei :: [Phoneme] -> [[Phoneme]] -> [Phoneme]
makeNuclei vows sonHier = vows

-- Generate valid codas
makeCodas :: [[Phoneme]] -> (Int, Int) -> RVar [[Phoneme]]
makeCodas sonHier (msd, maxCodaC) = do
  n <- uniform 5 10
  replicateM n (makeConsonantCluster (msd, maxCodaC) (reverse sonHier) [])

checkCC :: [Phoneme] -> Bool
checkCC cc = checkCC2 cc && length cc > 1

checkCC2 :: [Phoneme] -> Bool
checkCC2 [] = True
checkCC2 (p:ps) = fromMaybe True ((p /=) <$> headMay ps) && checkCC2 ps


-- This is like replicateM, except it replicates until it either hits the amount
-- you want or it reaches a specified limit (so it doesn't go forever).
-- This is actually pretty useful, I'm suprised it doesn't exist already.
replicateUntilM :: Monad m => (a -> Bool) -> Int -> Int -> m a -> m [a]
replicateUntilM _ 0 _ _ = return []
replicateUntilM _ _ 0 _ = return []
replicateUntilM test i limit thing = do
  revealThing <- thing
  itFails <- replicateUntilM test i (limit-1) thing
  itSucceeds <- replicateUntilM test (i-1) (limit-1) thing
  if test revealThing then return (revealThing:itSucceeds) else return itFails


-- Generate a consonant cluster
makeConsonantCluster :: (Int, Int) -> [[Phoneme]] -> [Phoneme] -> RVar [Phoneme]
makeConsonantCluster _ [] out = return out
makeConsonantCluster (_, 0) _ out = return out
makeConsonantCluster (msd, maxC) sonHier out = do
  (theRest, newC) <- maybe (return ([],[])) (fmap $ second (:[])) (join $ choiceExtract <$> headMay sonHier)
  let sonhierN = theRest : fromMaybe [] (tailMay sonHier)
  join $ choice [ makeConsonantCluster (msd, maxC-1) (drop msd sonhierN) (out ++ newC)
                , makeConsonantCluster (msd, maxC) (drop 1 sonhierN) out
                ]


-- Make sonority hierarchy from consonant inventory and scheme number
-- The hierarchies get more granular as scheme number increases
-- Hierarchies should be based on procedurally generated rules
-- That might play better with what I have in mind for phonotactics
makeSonHier :: [Phoneme] -> Int -> [[Phoneme]]
makeSonHier cns i = filter (not.null) scheme where
  (glides, cons2) = partition (\x -> cmanner x == APPROXIMANT && cplace x > PALATAL) cns
  (liquids, cons3) = partition (\x -> cmanner x `elem` [APPROXIMANT, LAPPROXIMANT, TRILL, FLAP, LFLAP]) cons2
  (nasals, cons4) = partition (\x -> cmanner x == NASAL) cons3
  (af, cons5) = partition (\x -> cmanner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && cvoice x == ASPIRATED) cons4
  (mf, cons6) = partition (\x -> cmanner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && cvoice x == MODAL) cons5
  (sf, cons7) = partition (\x -> cmanner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && cvoice x `elem` [SLACK, STIFF]) cons6
  (bf, cons8) = partition (\x -> cmanner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && cvoice x `elem` [BREATHY, CREAKY]) cons7
  (vf, cons9) = partition (\x -> cmanner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && cvoice x == VOICELESS) cons8
  (aa, cons10) = partition (\x -> cmanner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && cvoice x == ASPIRATED) cons9
  (ma, cons11) = partition (\x -> cmanner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && cvoice x == MODAL) cons10
  (sa, cons12) = partition (\x -> cmanner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && cvoice x `elem` [SLACK, STIFF]) cons11
  (ba, cons13) = partition (\x -> cmanner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && cvoice x `elem` [BREATHY, CREAKY]) cons12
  (va, cons14) = partition (\x -> cmanner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && cvoice x == VOICELESS) cons13
  (as, cons15) = partition (\x -> cmanner x == STOP && cvoice x == ASPIRATED) cons14
  (ms, cons16) = partition (\x -> cmanner x == STOP && cvoice x == MODAL) cons15
  (ss, cons17) = partition (\x -> cmanner x == STOP && cvoice x `elem` [SLACK, STIFF]) cons16
  (bs, cons18) = partition (\x -> cmanner x == STOP && cvoice x `elem` [BREATHY, CREAKY]) cons17
  (vs, other) = partition (\x -> cmanner x == STOP && cvoice x == VOICELESS) cons18

  aobstruent = concat [af, aa, as]
  mobstruent = concat [mf, ma, ms]
  sobstruent = concat [sf, sa, ss]
  bobstruent = concat [bf, ba, bs]
  vobstruent = concat [vf, va, vs]
  fricatives = concat [af, mf, sf, bf, vf]
  affricates = concat [aa, ma, sa, ba, va]
  stops = concat [as, ms, ss, bs, vs]
  obstruents = concat [fricatives, affricates, stops]

  scheme | i == 1 = [other, obstruents, nasals, liquids, glides]
         | i == 2 = [other, stops, affricates, fricatives, nasals, liquids, glides]
         | i == 3 = [other, vobstruent, bobstruent, sobstruent, mobstruent, aobstruent, nasals, liquids, glides]
         | i == 4 = [other, vs, bs, ss, ms, as, va, ba, sa, ma, aa, vf, bf, sf, mf, af, nasals, liquids, glides]
         | otherwise = []

-- Retrieve the sonority level of a given phoneme
-- The greater the number, the closer to "Vowel"
retrieveSon :: [[Phoneme]] -> Phoneme -> Int
retrieveSon sonHier Vowel{} = length sonHier + 1
retrieveSon sonHier Diphthong{} = length sonHier + 1
retrieveSon _ Blank = 0
retrieveSon sonhier p = fromMaybe 0 (elemIndex True $ map (elem p) sonhier)
