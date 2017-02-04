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

import Out.Other
import Out.Phonology
import Out.Inflection
import Out.Lexicon
import Out.Sentence
import Out.Grammar

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

  -- phonotactics
  sonHier <- sampleRVar (makeSonHier inventoryC)

  -- inflection / grammatical categories
  idata <- loadInputData
  (inflSys, numPerLexCat) <- sampleRVar (makeInflectionSystem idata)
  systems <- sampleRVar (mapM (makeLexicalInflection inventoryV sonHier inflSys) numPerLexCat)

  -- root morphemes
  mData <- loadMeaningData
  roots <- sampleRVar (makeRootDictionary mData (inventoryV ++ inventoryD) sonHier ((1, 4), (0, 2), (1, 3), (0, 2)))

  -- full Lexicon
  -- let dict = makeDictionary systems roots

  -- grammar
  grammar <- sampleRVar makeGrammar

  -- parse trees
  ptExamples <- sampleRVar (replicateM 10 (makeParseTree mData))

  -- characters
  chracterSVGs <- sampleRVar (makeCharacters 5)

  -- outputs
  writeFile "out/phonology.txt" $ "Phonology"
                               ++ parseConPhonemeInventory places manners phonations inventoryC
                               ++ parseVowPhonemeInventory heights backs rounds lengths inventoryV
                               ++ parseDiphPhonemeInventory inventoryD

  writeFile "out/phonotactics.txt" $ "Phonotactics"
                                  ++ parseSonHier (inventoryV ++ inventoryD) sonHier

  writeFile "out/inflection.txt" $ "Inflection"
                                ++ parseLCInflection inflSys
                                ++ concatMap (parseLexicalSystems inflSys sonHier) systems

  writeFile "out/lexicon.txt" $ "Lexicon"
                             -- ++ parseDictionary sonHier dict

  writeFile "out/grammar.txt" $ "Grammar"
                             ++ parseGrammar grammar
                            -- ++ concatMap (parseParseTree sonHier roots systems grammar) ptExamples

  writeFile "out/character.svg" $  intercalate "\n" chracterSVGs
