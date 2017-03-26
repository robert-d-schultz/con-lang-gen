module Gen.Language
( makeLanguage
) where

import Data.RVar
import Data.List

import LoadStuff

import Gen.Phonology
import Gen.Phonotactics
import Gen.Root
import Gen.Word
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
  places <- makePlaces
  manners <- makeManners
  phonations <- makePhonations
  inventoryC <- makeConsonants places manners phonations

  -- vowels
  heights <- makeHeights
  backs <- makeBacknesses
  rounds <- makeRoundedneses
  lengths <- makeLengths
  tones <- makeTones
  inventoryV <- makeVowels heights backs rounds lengths tones

  -- diphthongs
  inventoryD <- makeDiphInventory 4 inventoryV

  -- phonotactics / consonant clusters
  sonHier <- makeSonHier inventoryC
  onsetCCs <- makeOnsets sonHier (2, 4)
  codaCCs <- makeCodas sonHier (2, 4)

  let onsets = nub $ onsetCCs ++ map (:[]) inventoryC
  let codas = nub $ codaCCs ++ map (:[]) inventoryC
  let nucleuss = inventoryV ++ inventoryD

  -- inflection / grammatical categories
  (inflSys, numPerLexCat) <- makeInflectionSystem idata
  systems <- mapM (makeLexicalInflection nucleuss (onsets, codas) inflSys) numPerLexCat

  -- root morphemes
  roots <- makeRootDictionary mData nucleuss (onsets, codas) (1, 4)

  -- grammar
  grammar <- makeGrammar

  -- writing systems
  let allPhonemes = inventoryD ++ inventoryV ++ inventoryC
  let allSyllables | 2000 < product [length onsets, length nucleuss, length codas] = []
                   | otherwise = makeAllSyllables onsets nucleuss codas
  let allLogograms = roots
  (a, s, l) <- generateWritingSystem allPhonemes allSyllables allLogograms

  -- chracters
  (aOut, sOut, lOut) <- makeCharacters (a, s, l)

  -- find out what was assigned to "<!LANGUAGE!>"
  let nameMorph = snd $ head (filter (\x -> fst x == ("<!LANGUAGE!>", Noun)) roots)

  let langName = romanizeMorpheme nameMorph

  let lang = Language langName inventoryC inventoryV inventoryD sonHier onsetCCs codaCCs inflSys systems grammar roots (aOut, sOut, lOut)

  return lang
