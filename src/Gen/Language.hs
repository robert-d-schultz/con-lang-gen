module Gen.Language
( makeLanguage
) where

import ClassyPrelude

import Data.Random hiding (sample)
import Data.Random.Extras

import LoadStuff

import Gen.Phonology
import Gen.Phonotactics
import Gen.Root
import Gen.Inflection
import Gen.Morphology
import Gen.Grammar
import Gen.ParseTree

import Gen.Grapheme
import Gen.WritingSystem

import Data.Language
import Data.Phoneme
import Data.Word
import Data.Inflection
import Data.Soundchange

import Out.Roman
import Out.Sentence

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
  let onsets = ordNub $ onsetCCs ++ map (:[]) inventoryC

  nuclei <- makeNuclei (inventoryV ++ inventoryD) sonHier

  maxCoda <- uniform 0 6
  numCodaCCs <- uniform 0 5
  codaCCs <- makeCodas numCodaCCs 0 [] onsets inventoryC sonHier (msd, maxCoda)
  let codas = ordNub $ [] : codaCCs  -- makes sure "no coda" is always allowed

  -- inflection / grammatical categories
  (inflSys, numPerLexCat) <- makeInflectionMap idata
  inflMorphs <- concat <$> mapM (makeLexicalInflection onsets nuclei codas tones inflSys) numPerLexCat

  -- Pick lemma morphemes to be used in dictionary
  lemmaMorphs <- concat <$> mapM (pickLemmaMorphemes inflSys inflMorphs) [Verb .. Pron]

  -- Root morphemes
  rootMorphs <- makeRootDictionary mData onsets nuclei codas tones (1, 4) numPerLexCat

  -- Grammar
  grammar <- makeGrammar

  -- Writing systems
  let allPhonemes = inventoryD ++ inventoryV ++ inventoryC
  let allSyllables | 2000 < product [length onsets, length nuclei, length codas] = []
                   | otherwise = makeAllSyllables onsets nuclei codas tones [NONES, PRIMARYS, SECONDARYS]
  let allLogograms = rootMorphs ++ inflMorphs
  (a, s, l) <- generateWritingSystem allPhonemes allSyllables allLogograms

  -- Characters
  (aOut, sOut, lOut) <- makeCharacters (a, s, l)

  -- Find out what was assigned to "!!!LANGUAGE!!!" and romanize
  let langName = fromMaybe "name not found" (romanizeWord <$> find (\x -> getMeaning x == Meaning Noun "!!!LANGUAGE!!!") rootMorphs)

  let lang = Language langName ("", "") inventoryC inventoryV inventoryD (places, manners, phonations, airstreams) (heights, backs, rounds, lengths) tones scheme onsets nuclei codas inflSys inflMorphs lemmaMorphs rootMorphs grammar (aOut, sOut, lOut) [NoChange]

  return lang

-- This picks the lemma morphemes
pickLemmaMorphemes :: InflectionMap -> [Morpheme] -> LexCat -> RVar [Morpheme]
pickLemmaMorphemes inflSys inflMorphs lc = do
  -- Generate a random AllExpress for this LexCat
  lemmaExpress <- generateInflection inflSys lc
  -- Filter the master list of inflection Morpheme's, this LexCat only
  let relMorphs = filter (\x -> lc == getLC (getMeaning x)) inflMorphs
  -- Filter down to the ones that help satisfy the lemmaExpress
  let lemmaMorphs = filter (\x -> compareInfl (getAllExpress $ getMeaning x) lemmaExpress) relMorphs
  return lemmaMorphs
