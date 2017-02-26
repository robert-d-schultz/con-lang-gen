module Main where

import Data.Random.Extras hiding (sample)
import Data.Random
import Data.RVar
import Data.List
import Data.Hashable
import Control.Monad
import System.Random
import System.Directory

import LoadStuff

import Gen.Phonology
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
  -- seed
  input <- putStr "Enter seed: " *> getLine
  let seed = hashWithSalt 1 input
  setStdGen $ mkStdGen seed

  exist <- doesPathExist $ "out/" ++ show seed
  if exist then putStrLn "Language already generated" *> main else
    generateLanguage seed

-- given a seed, generate a language
generateLanguage :: Int -> IO ()
generateLanguage seed = do
  -- consonants
  places <- newSample makePlaces
  manners <- newSample makeManners
  phonations <- newSample makePhonations
  inventoryC <- newSample (makeConsonants places manners phonations)

  -- vowels
  heights <- newSample makeHeights
  backs <- newSample makeBacknesses
  rounds <- newSample makeRoundedneses
  lengths <- newSample makeLengths
  tones <- newSample makeTones
  inventoryV <- newSample (makeVowels heights backs rounds lengths tones)

  -- diphthongs
  inventoryD <- newSample (makeDiphInventory 4 inventoryV)

  -- phonotactics / consonant clusters
  sonHier <- newSample (makeSonHier inventoryC)
  onsetCCs <- newSample (makeOnsets sonHier (2, 4))
  codaCCs <- newSample (makeCodas sonHier (2, 4))

  let onsets = nub $ onsetCCs ++ map (:[]) inventoryC
  let codas = nub $ codaCCs ++ map (:[]) inventoryC
  let nucleuss = inventoryV ++ inventoryD


  -- inflection / grammatical categories
  idata <- loadInputData
  (inflSys, numPerLexCat) <- newSample (makeInflectionSystem idata)
  systems <- newSample (mapM (makeLexicalInflection nucleuss (onsets, codas) inflSys) numPerLexCat)

  -- root morphemes
  mData <- loadMeaningData
  roots <- newSample (makeRootDictionary mData nucleuss (onsets, codas) (1, 4))

  -- full Lexicon
  -- let dict = makeDictionary systems roots

  -- grammar
  grammar <- newSample makeGrammar

  -- parse trees
  ptExamples <- newSample (replicateM 5 (makeParseTree mData))

  -- writing systems
  let allPhonemes = inventoryD ++ inventoryV ++ inventoryC
  let allSyllables | 2000 < product [length onsets, length nucleuss, length codas] = []
                   | otherwise = makeAllSyllables onsets nucleuss codas
  let allLogograms = roots
  (a, s, l) <- newSample (generateWritingSystem allPhonemes allSyllables allLogograms)

  -- chracters
  (aOut, sOut, lOut) <- newSample $ makeCharacters (a, s, l)

  let characterSVGs = map snd aOut ++ map snd sOut ++ map snd lOut

  -- outputs
  createDirectory $ "out/" ++ show seed
  writeFile ("out/" ++ show seed ++ "/phonology.txt") $ "Phonology"
                               ++ parseConPhonemeInventory places manners phonations inventoryC
                               ++ parseVowPhonemeInventory heights backs rounds lengths inventoryV
                               ++ parseDiphPhonemeInventory inventoryD

  writeFile ("out/" ++ show seed ++ "/phonotactics.txt") $ "Phonotactics"
                                  ++ parseSonHier (inventoryV ++ inventoryD) sonHier
                                  ++ parseCCs onsets codas

  writeFile ("out/" ++ show seed ++ "/inflection.txt") $ "Inflection"
                                ++ parseLCInflection inflSys
                                ++ concatMap (parseLexicalSystems inflSys sonHier) systems

  writeFile ("out/" ++ show seed ++ "/lexicon.txt") $ "Lexicon"
                            -- ++ parseDictionary sonHier dict
                             ++ parseRootDictionary sonHier roots

  writeFile ("out/" ++ show seed ++ "/grammar.txt") $ "Grammar"
                             ++ parseGrammar grammar
                             ++ concatMap (parseParseTree sonHier roots systems grammar) ptExamples

  writeFile ("out/" ++ show seed ++ "/writing system.txt") $ "Writing System"
                                    ++ parseWritingSystem (aOut, sOut, lOut)

  createDirectory $ "out/" ++ show seed ++ "/characters"
  let t = map (\(num, str) -> ("out/" ++ show seed ++ "/characters/U+" ++ show num ++ ".svg", str)) characterSVGs
  forM_ t $ uncurry writeFile

-- special sampleRVar that allows seeds and shit
newSample :: RVar a -> IO a
newSample i = do
  g1 <- getStdGen
  let out = sampleState i g1
  setStdGen $ snd out
  return $ fst out
