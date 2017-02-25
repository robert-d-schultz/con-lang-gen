module Main where

import Data.Random.Extras
import Data.Random hiding (sample)
import Data.RVar
import Control.Monad
import Data.List

import LoadStuff

import Gen.Phoneme
import Gen.Phonotactics
import Gen.Root
import Gen.Word
import Gen.Inflection
import Gen.Morphology
import Gen.Grammar
import Gen.ParseTree

import Gen.Grapheme
import Gen.WritingSystem

import Out.Other
import Out.Phonology
import Out.Inflection
import Out.Lexicon
import Out.Sentence
import Out.Grammar
import Out.WritingSystem

main :: IO ()
main = do

  -- consonants
  places <- sampleRVar makePlaces
  manners <- sampleRVar makeManners
  phonations <- sampleRVar makePhonations
  inventoryC <- sampleRVar (makeConsonants places manners phonations)

  -- vowels
  heights <- sampleRVar makeHeights
  backs <- sampleRVar makeBacknesses
  rounds <- sampleRVar makeRoundedneses
  lengths <- sampleRVar makeLengths
  tones <- sampleRVar makeTones
  inventoryV <- sampleRVar (makeVowels heights backs rounds lengths tones)

  -- diphthongs
  inventoryD <- sampleRVar (makeDiphInventory 4 inventoryV)

  -- phonotactics / consonant clusters
  sonHier <- sampleRVar (makeSonHier inventoryC)
  onsets <- sampleRVar (makeOnsets sonHier (2, 4))
  codas <- sampleRVar (makeCodas sonHier (2, 4))

  -- inflection / grammatical categories
  idata <- loadInputData
  (inflSys, numPerLexCat) <- sampleRVar (makeInflectionSystem idata)
  systems <- sampleRVar (mapM (makeLexicalInflection inventoryV (onsets, codas) inflSys) numPerLexCat)

  -- root morphemes
  mData <- loadMeaningData
  roots <- sampleRVar (makeRootDictionary mData (inventoryV ++ inventoryD) (onsets ++ map (:[]) inventoryC, codas ++ map (:[]) inventoryC) (1, 4))

  -- full Lexicon
  -- let dict = makeDictionary systems roots

  -- grammar
  grammar <- sampleRVar makeGrammar

  -- parse trees
  ptExamples <- sampleRVar (replicateM 10 (makeParseTree mData))

  -- writing systems
  let allPhonemes = inventoryD ++ inventoryV ++ inventoryC
  let allSyllables = makeAllSyllables (onsets ++ map (:[]) inventoryC) (inventoryD ++ inventoryV) (codas ++ map (:[]) inventoryC)
  let allLogograms = roots
  (a, s, l) <- sampleRVar (generateWritingSystem allPhonemes allSyllables allLogograms)

  -- chracters
  (aOut, sOut, lOut) <- sampleRVar $ makeCharacters (a, s, l)

  let characterSVGs = map snd aOut ++ map snd sOut ++ map snd lOut

  -- outputs
  writeFile "out/phonology.txt" $ "Phonology"
                               ++ parseConPhonemeInventory places manners phonations inventoryC
                               ++ parseVowPhonemeInventory heights backs rounds lengths inventoryV
                               ++ parseDiphPhonemeInventory inventoryD

  writeFile "out/phonotactics.txt" $ "Phonotactics"
                                  ++ parseSonHier (inventoryV ++ inventoryD) sonHier
                                  ++ parseCCs onsets codas

  writeFile "out/inflection.txt" $ "Inflection"
                                ++ parseLCInflection inflSys
                                ++ concatMap (parseLexicalSystems inflSys sonHier) systems

  writeFile "out/lexicon.txt" $ "Lexicon"
                            -- ++ parseDictionary sonHier dict
                             ++ parseRootDictionary sonHier roots

  writeFile "out/grammar.txt" $ "Grammar"
                             ++ parseGrammar grammar
                            -- ++ concatMap (parseParseTree sonHier roots systems grammar) ptExamples

  writeFile "out/writing system.txt" $ "Writing System"
                                    ++ parseWritingSystem (aOut, sOut, lOut)
  let t = map (\(num, str) -> ("out/characters/U+" ++ show num ++ ".svg", str)) characterSVGs
  forM_ t $ uncurry writeFile
