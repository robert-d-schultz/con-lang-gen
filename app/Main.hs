module Main where

import Data.Random
import Data.RVar
import Data.List
import Data.Hashable
import Control.Monad
import System.Random
import System.Directory

import LoadStuff

import Gen.Language
import Gen.LanguageTree
import Gen.Phonology
import Gen.Phonotactics
import Gen.ParseTree

import Out.Other
import Out.Phonology
import Out.Inflection
import Out.Lexicon
import Out.Sentence
import Out.Grammar
import Out.WritingSystem

import Data.Other

main :: IO ()
main = do
  -- seed
  input <- putStr "Enter seed: " *> getLine
  let seed = hashWithSalt 1 input
  setStdGen $ mkStdGen seed

  exist <- doesPathExist $ "out/" ++ show seed
  if exist then putStrLn "Language family already generated" *> main else do
    idata <- loadInputData
    mData <- loadMeaningData
    tree <- newSample $ makeLanguageTree 0 idata mData RootL
    parseLanguageTree seed mData tree

-- outputs stuff for each language family
parseLanguageTree :: Int -> MeaningData -> LanguageBranch -> IO()
parseLanguageTree seed mData tree = do
  let dir = "out/" ++ show seed ++ "/"
  createDirectory dir

  let treeMap = parseLanguageTreeN tree
  writeFile (dir ++ "tree_map.html") treeMap
  parseLanguageBranch seed mData tree

parseLanguageBranch :: Int -> MeaningData -> LanguageBranch -> IO()
parseLanguageBranch seed mData (LanguageBranch lang [] _) = parseLanguage lang seed mData
parseLanguageBranch seed mData (LanguageBranch lang branches _) = parseLanguage lang seed mData <* mapM_ (parseLanguageBranch seed mData) branches

-- outputs stuff for each language
parseLanguage :: Language -> Int -> MeaningData -> IO()
parseLanguage lang seed mData = do
  let (places, manners, phonations, exceptionsC) = getCMap lang
  let (heights, backs, rounds, lengths, tones, exceptionsV) = getVMap lang
  let inventoryD = getDs lang
  let onsetCCs = getOnsetCCs lang
  let codaCCs = getCodaCCs lang
  let scheme = getSonHier lang
  let grammar = getGrammar lang
  let inflSys = getInflMap lang
  let systems = getManSyss lang
  let roots = getRoots lang
  let (alph, syll, logo) = getWriting lang
  let name = getName lang

  let inventoryC = makeConsonants places manners phonations exceptionsC
  let inventoryV = makeVowels heights backs rounds lengths tones exceptionsV

  let sonHier = makeSonHier_ inventoryC scheme

  let onsets = nub $ onsetCCs ++ map (:[]) inventoryC
  let codas = nub $ codaCCs ++ map (:[]) inventoryC
  let nucleuss = inventoryV ++ inventoryD

  -- writing
  let characterSVGs = map snd alph ++ map snd syll ++ map snd logo

  -- parse trees
  ptExamples <- sampleRVar $ replicateM 5 (makeParseTree mData)

  -- make directory
  dir <- makeDirectory seed name 0
  createDirectory dir

  -- write to file a bunch of stuff
  writeFile (dir ++ "phonology.html") $ "Phonology"
                                 ++ parseConPhonemeInventory inventoryC
                                 ++ parseVowPhonemeInventory inventoryV
                                 ++ parseDiphPhonemeInventory inventoryD

  writeFile (dir ++ "phonotactics.html") $ "Phonotactics"
                                 ++ parseSonHier (inventoryV ++ inventoryD) sonHier
                                 ++ parseCCs onsets codas

  writeFile (dir ++ "inflection.html") $ "Inflection"
                                 ++ parseLCInflection inflSys
                                 ++ parseLexicalSystems inflSys sonHier systems

  writeFile (dir ++ "lexicon.html") $ "Lexicon"
                              -- ++ parseDictionary sonHier dict
                                 ++ parseRootDictionary sonHier roots

  writeFile (dir ++ "grammar.html") $ "Grammar"
                                 ++ parseGrammar grammar
                                 ++ concatMap (parseParseTree sonHier roots systems grammar) ptExamples

  writeFile (dir ++ "writing system.html") $ "Writing System"
                                 ++ parseWritingSystem (alph, syll, logo)

  -- createDirectory $ dir + "characters"
  -- let t = map (\(num, str) -> (dir ++ "characters/U+" ++ show num ++ ".svg", str)) characterSVGs
  -- forM_ t $ uncurry writeFile

-- used when multiple languages share the same name
makeDirectory :: Int -> String -> Int -> IO String
makeDirectory seed name 0 = do
  let dir = "out/" ++ show seed ++ "/" ++ name ++ "/"
  exist <- doesPathExist dir
  if exist then makeDirectory seed name 1 else return dir
makeDirectory seed name count = do
  let dir = "out/" ++ show seed ++ "/" ++ name ++ show count ++ "/"
  exist <- doesPathExist dir
  if exist then makeDirectory seed name (count + 1) else return dir

-- special sampleRVar that allows seeds and shit
newSample :: RVar a -> IO a
newSample i = do
  g1 <- getStdGen
  let out = sampleState i g1
  setStdGen $ snd out
  return $ fst out
