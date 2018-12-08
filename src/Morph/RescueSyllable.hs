module Morph.RescueSyllable
( rescueSyllables
) where

import ClassyPrelude

import Data.List (nub)

import Data.Language
import Data.Phoneme

import Constants

-- Corrects invalid syllables
-- Redistribute phonemes, or introduce syllables to correct these problems
rescueSyllables :: Language -> [Syllable] -> [Syllable]
rescueSyllables lang sylls = rescueSyllables_ lang [] sylls_ where
  sylls_ = map (rescueNucleus lang) sylls

rescueSyllables_ :: Language -> [Syllable] -> [Syllable] -> [Syllable]
rescueSyllables_ _ ys [] = ys
rescueSyllables_ lang ys xs = out where
  ysN = rescueSyllable lang (lastMay ys) (headMay xs)
  out = rescueSyllables_ lang (fromMaybe [] (initMay ys) ++ ysN) (fromMaybe [] $ tailMay xs)

-- Replaces nucleus with mid-central-ish vowel if it's invalid
-- Pushes syllabic consonants to the onset or coda if possible
rescueNucleus :: Language -> Syllable -> Syllable
rescueNucleus lang syll@(Syllable onset nuc coda _ _)
  | getNucleus syll `elem` getNuclei lang = syll
  | (\case Vowel{} -> True; _ -> False) nuc = syll{getNucleus = getCenterVowel lang}
  | (onset ++ [nuc]) `elem` getOnsetCCs lang = syll{getOnset = onset ++ [nuc], getNucleus = getCenterVowel lang}
  | (nuc:coda) `elem` getCodaCCs lang = syll{getNucleus = getCenterVowel lang, getCoda = nuc:coda}
  | otherwise = syll{getNucleus = getCenterVowel lang}

-- Looks at coda of syll1 and onset of syll2 and checks/fixes them
rescueSyllable :: Language -> Maybe Syllable -> Maybe Syllable -> [Syllable]
rescueSyllable lang Nothing Nothing = []
rescueSyllable lang Nothing (Just syll2) = splitOffOnset lang 0 syll2
rescueSyllable lang (Just syll1) Nothing = splitOffCoda lang 0 syll1
rescueSyllable lang (Just syll1) (Just syll2) = out where
    coda = getCoda syll1
    onset = getOnset syll2

    foobar = do
      let (codaN, rest, onsetN) = satisfyCodaOnset lang (coda ++ onset)
      let restN = makeSyllablesFromCC lang rest
      [syll1{getCoda = codaN}] ++ restN ++ [syll2{getOnset = onsetN}]

    out | onset `elem` getOnsetCCs lang && coda `elem` getCodaCCs lang = [syll1, syll2]
        | otherwise = fromMaybe foobar (do
            (codaN, onsetN) <- transferCodaOnset lang 0 (coda ++ onset)
            return [syll1{getCoda = codaN}, syll2{getOnset = onsetN}])

-- Tries to transfer phonemes between coda and onset
-- Returns  Nothing if that doesn't work
transferCodaOnset :: Language -> Int -> ConsCluster -> Maybe (ConsCluster, ConsCluster)
transferCodaOnset lang i cs = out where
  (codaN, onsetN) = splitAt i cs
  out
    | i > length cs = Nothing
    | onsetN `elem` getOnsetCCs lang && codaN `elem` getCodaCCs lang = Just (codaN, onsetN)
    | otherwise = transferCodaOnset lang (i+1) cs

-- Tries to satisfy coda and onset, and returns anything that is left over
satisfyCodaOnset :: Language -> ConsCluster -> (ConsCluster, ConsCluster, ConsCluster)
satisfyCodaOnset lang cs = (codaN, rest_, onsetN) where

  (rest, onsetN) = satisfyOnset lang 0 cs
  (codaN, rest_) = satisfyCoda lang 0 rest

satisfyOnset :: Language -> Int -> ConsCluster -> (ConsCluster, ConsCluster)
satisfyOnset lang i cs = out where
  (rest, onset) = splitAt i cs
  out
    | i > length cs = (cs, [])
    | onset `elem` getOnsetCCs lang = (rest, onset)
    | otherwise = satisfyOnset lang (i+1) cs

satisfyCoda :: Language -> Int -> ConsCluster -> (ConsCluster, ConsCluster)
satisfyCoda lang i cs = out where
  (coda, rest) = splitAt (length cs - i) cs
  out
    | i < 0 = ([], cs)
    | coda `elem` getCodaCCs lang = (coda, rest)
    | otherwise = satisfyCoda lang (i+1) cs

-- Splits off invalid Onsets into new syllables
-- Finds the maximal valid onset for a given syllable
-- Takes the rest of the onset and makes syllables out of it
splitOffOnset :: Language -> Int -> Syllable -> [Syllable]
splitOffOnset lang i syll@(Syllable onset _ _ _ _) = out where
  onsets = getOnsetCCs lang
  (rest, onsetN) = splitAt i onset
  out
    | i > length onset = [syll] -- this means it failed, basically
    | onsetN `elem` onsets = makeSyllablesFromCC lang rest ++ [syll{getOnset = onsetN}]
    | otherwise = splitOffOnset lang (i+1) syll

-- Splits off invalid Codas into new syllables
-- Finds the maximal valid coda for a given syllable
-- Takes the rest of the coda and makes syllables out of it
-- This should only be used for a word's final syllable
splitOffCoda :: Language -> Int -> Syllable -> [Syllable]
splitOffCoda lang i syll@(Syllable _ _ coda _ _) = out where
  codas = getCodaCCs lang
  (codaN, rest) = splitAt (length coda - i) coda
  out
    | i < 0 = [syll] -- this means it failed, basically
    | codaN `elem` codas = syll{getCoda = codaN} : makeSyllablesFromCC lang rest
    | otherwise = splitOffCoda lang (i+1) syll

-- Make syllables out of the rest of the onset
makeSyllablesFromCC :: Language -> ConsCluster -> [Syllable]
makeSyllablesFromCC lang cc = out where
  filt = filter (all (\(o,c) -> o `elem` getOnsetCCs lang && c `elem` getCodaCCs lang)) (superSplit lang cc)
  ocs = filter (\x -> length x == length (minimumByEx (comparing length) filt)) filt -- fewest additional syllables
  ocs_ = maximumByEx (comparing (sum.map (\(a,b) -> length (a :: ConsCluster)))) ocs -- maximize onsets

  nuc = getCenterVowel lang
  out = map (\(o,c) -> Syllable o nuc c NONET NONES) ocs_

-- Returns all possible ways to split a list into 2 parts, and then those 2 parts into 2 parts, etc.
-- Probably explained it badly there
-- This could probably be a lot more efficient, kinda slow past 6 phonemes, and using "nub" is a hack
superSplit :: Language -> ConsCluster -> [[(ConsCluster, ConsCluster)]]
superSplit lang [] = []
superSplit lang cs = nub bar where
  foo = map (`splitAt` cs) [0..(length cs)]
  filt = filter (\(a,b) -> a `elem` getOnsetCCs lang && b `elem` getCodaCCs lang) foo
  bar | null filt = concatMap (\(a,b) -> if null a ||  null b then [[(a,b)]] else (++) <$> superSplit lang a <*> superSplit lang b) foo
      | otherwise = map (:[]) filt -- if you find at least one way of dividing the syllable, go no farther

-- Returns the most center and mid-ish vowel in a language's vowel inventory
-- Used to rescue invalid onset/codas
getCenterVowel :: Language -> Phoneme
getCenterVowel lang = unsafeHead out where
  vowels = getVInv lang
  out = sortBy (comparing (phonemeDistance (Vowel MID CENTRAL UNROUNDED NORMAL))) vowels
