{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
module Gen.Language
( makeLanguage
) where

import ClassyPrelude

import Data.Random hiding (sample)

import LoadStuff

import Gen.Phonology
import Gen.Phonotactics
import Gen.Root
import Gen.Inflection
import Gen.Morphology
import Gen.Grammar

import Gen.Grapheme
import Gen.WritingSystem

import Data.Other
import Data.Inflection

import Out.Roman

-- generates a language
makeLanguage :: InputData -> MeaningData -> RVar Language
makeLanguage idata mData = do
  -- consonants
  (places, manners, phonations, exceptionsC) <- makeConsonantMap
  let inventoryC = makeConsonants places manners phonations exceptionsC

  -- vowels
  (heights, backs, rounds, lengths, tones, exceptionsV) <- makeVowelMap
  let inventoryV = makeVowels heights backs rounds lengths tones exceptionsV

  -- diphthongs
  inventoryD <- makeDiphInventory 4 inventoryV

  -- phonotactics / consonant clusters
  scheme <- uniform 1 4
  let sonHier = makeSonHier inventoryC scheme
  msd <- uniform 0 3

  maxOnset <- uniform 0 6
  onsetCCs <- makeOnsets sonHier (msd, maxOnset)
  let onsets = ordNub $ onsetCCs ++ map (:[]) inventoryC

  let nuclei = makeNuclei (inventoryV ++ inventoryD) sonHier

  maxCoda <- uniform 0 6
  codaCCs <- makeCodas sonHier (msd, maxCoda)
  let codas = ordNub $ [] : codaCCs ++ map (:[]) inventoryC  -- makes sure "no coda" is always allowed




  -- inflection / grammatical categories
  (inflSys, numPerLexCat) <- makeInflectionMap idata
  systems <- concat <$> mapM (makeLexicalInflection nuclei (onsets, codas) inflSys) numPerLexCat

  -- root morphemes
  roots <- makeRootDictionary mData nuclei (onsets, codas) (1, 4)

  -- grammar
  grammar <- makeGrammar

  -- writing systems
  let allPhonemes = inventoryD ++ inventoryV ++ inventoryC
  let allSyllables | 2000 < product [length onsets, length nuclei, length codas] = []
                   | otherwise = makeAllSyllables onsets nuclei codas
  let allLogograms = roots
  (a, s, l) <- generateWritingSystem allPhonemes allSyllables allLogograms

  -- characters
  (aOut, sOut, lOut) <- makeCharacters (a, s, l)

  -- find out what was assigned to "<!LANGUAGE!>" and romanize
  let langName = fromMaybe "name not found" (romanizeMorpheme . snd <$> find (\x -> fst x == ("<!LANGUAGE!>", Noun)) roots)

  let lang = Language langName (places, manners, phonations) inventoryC (heights, backs, rounds, lengths, tones) inventoryV inventoryD scheme onsets nuclei codas inflSys systems grammar roots (aOut, sOut, lOut)

  return lang
