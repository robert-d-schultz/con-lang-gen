module Gen.Language
( makeLanguage
) where

import ClassyPrelude

import Data.Random hiding (sample, shuffle)
import Data.Random.Extras

import LoadStuff

import Gen.Phonology
import Gen.Phonotactics
import Gen.Inflection
import Gen.Morpheme
import Gen.Grammar

import Gen.Grapheme
import Gen.WritingSystem

import Data.Language
import Data.Phoneme
import Data.Word
import Data.Inflection
import Data.Soundchange

import Out.Roman

-- generates a language
makeLanguage :: InputData -> MeaningData -> RVar Language
makeLanguage idata mData = do
  -- consonants
  (places, manners, phonations, airstreams, exceptionsC) <- makeConsonantMap
  let inventoryC = makeConsonants places manners phonations airstreams exceptionsC

  -- vowels
  (heights, backs, rounds, lengths, exceptionsV) <- makeVowelMap
  let inventoryV = makeVowels heights backs rounds lengths exceptionsV

  -- diphthongs
  inventoryD <- makeDiphInventory 4 inventoryV

  -- tones
  tones <- makeTones

  -- phonotactics / consonant clusters
  scheme <- uniform 5 5 -- fixed to be the ssp-violating scheme
  let sonHier = makeSonHier inventoryC scheme
  msd <- uniform 0 3

  maxOnset <- uniform 0 6
  onsetCCs <- makeOnsets sonHier (msd, maxOnset)
  let onsets_ = ordNub $ onsetCCs ++ map (:[]) inventoryC
  onsets <- shuffle onsets_ -- shuffle to allow triangular distribution

  nuclei_ <- makeNuclei (inventoryV ++ inventoryD) sonHier
  nuclei <- shuffle nuclei_ -- shuffle to allow triangular distribution

  maxCoda <- uniform 0 6
  numCodaCCs <- uniform 0 5
  codaCCs <- makeCodas numCodaCCs 0 [] onsets inventoryC sonHier (msd, maxCoda)
  codas_ <- shuffle codaCCs -- shuffle to allow triangular distribution
  let codas = [] : codas_  -- makes sure "no coda" is always allowed


  -- inflection / grammatical categories
  (inflSys, numPerLexCat) <- makeInflectionMap idata
  inflMorphs <- concat <$> mapM (\x -> makeInflectionMorphemes onsets nuclei codas tones inflSys x (1, 4)) numPerLexCat

  -- Pick lemma morphemes to be used in dictionary
  lemmaMorphs <- concat <$> mapM (pickLemmaMorphemes inflSys inflMorphs) [Verb .. Pron]

  -- Derivational morphemes
  derivMorphs <- makeDerivationMorphemes mData onsets nuclei codas tones (1, 4)

  -- Root morphemes
  rootMorphs <- makeRootMorphemes mData onsets nuclei codas tones (1, 4) numPerLexCat

  -- Grammar
  grammar <- makeGrammar

  -- Writing systems
  let allPhonemes = inventoryD ++ inventoryV ++ inventoryC
  let allSyllables | 2000 < product [length onsets, length nuclei, length codas, 3] = []
                   | otherwise = makeAllSyllables onsets nuclei codas tones [NONES, PRIMARYS, SECONDARYS]
  let allLogograms = rootMorphs ++ derivMorphs ++ inflMorphs
  (a, s, l) <- generateWritingSystem allPhonemes allSyllables allLogograms

  -- Characters
  (aOut, sOut, lOut) <- makeCharacters (a, s, l)

  -- Find out what was assigned to "!!!LANGUAGE!!!" and romanize
  let langName = fromMaybe "name not found" (romanizeWord <$> find (\x -> getMeaning x == Meaning Noun "!!!LANGUAGE!!!") rootMorphs)

  let lang = Language langName ("", "") inventoryC inventoryV inventoryD (places, manners, phonations, airstreams) (heights, backs, rounds, lengths) tones scheme onsets nuclei codas inflSys inflMorphs lemmaMorphs derivMorphs rootMorphs grammar (aOut, sOut, lOut) [NoChange]

  return lang
