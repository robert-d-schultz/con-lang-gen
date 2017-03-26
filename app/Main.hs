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

import Gen.Language
import Gen.LanguageTree
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
  if exist then putStrLn "Language tree already generated" *> main else do
    idata <- loadInputData
    mData <- loadMeaningData
    tree <- newSample $ makeLanguageTree 0 idata mData RootL
    parseLanguageTree seed mData tree

parseLanguageTree :: Int -> MeaningData -> LanguageBranch -> IO()
parseLanguageTree seed mData tree = do
  let dir = "out/" ++ show seed ++ "/"
  createDirectory dir

  let treeMap = parseLanguageTree2 tree
  writeFile (dir ++ "tree_map.html") treeMap
  parseLanguageBranch seed mData tree

parseLanguageBranch :: Int -> MeaningData -> LanguageBranch -> IO()
parseLanguageBranch seed mData (LanguageBranch lang [] _) = parseLanguage lang seed mData
parseLanguageBranch seed mData (LanguageBranch lang branches _) = parseLanguage lang seed mData <* mapM_ (parseLanguageBranch seed mData) branches

-- outputs a bunch of stuff
parseLanguage :: Language -> Int -> MeaningData -> IO()
parseLanguage lang seed mData = do
  let inventoryC = getCs lang
  let inventoryV = getVs lang
  let inventoryD = getDs lang
  let onsetCCs = getOnsetCCs lang
  let codaCCs = getCodaCCs lang
  let sonHier = getSonHier lang
  let grammar = getGrammar lang
  let inflSys = getInflSys lang
  let systems = getInflSyss lang
  let roots = getRoots lang
  let (alph, syll, logo) = getWriting lang
  let name = getName lang

  let onsets = nub $ onsetCCs ++ map (:[]) inventoryC
  let codas = nub $ codaCCs ++ map (:[]) inventoryC
  let nucleuss = inventoryV ++ inventoryD


  -- writing
  let characterSVGs = map snd alph ++ map snd syll ++ map snd logo

  -- parse trees
  ptExamples <- sampleRVar $ replicateM 5 (makeParseTree mData)

  -- outputs
  let dir = "out/" ++ show seed ++ "/" ++ name ++ "/"
  createDirectory dir
  writeFile (dir ++ "phonology.html") $ "Phonology"
                                 ++ parseConPhonemeInventory inventoryC
                                 ++ parseVowPhonemeInventory inventoryV
                                 ++ parseDiphPhonemeInventory inventoryD

  writeFile (dir ++ "phonotactics.html") $ "Phonotactics"
                                 ++ parseSonHier (inventoryV ++ inventoryD) sonHier
                                 ++ parseCCs onsets codas

  writeFile (dir ++ "inflection.html") $ "Inflection"
                                 ++ parseLCInflection inflSys
                                 ++ concatMap (parseLexicalSystems inflSys sonHier) systems

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

-- special sampleRVar that allows seeds and shit
newSample :: RVar a -> IO a
newSample i = do
  g1 <- getStdGen
  let out = sampleState i g1
  setStdGen $ snd out
  return $ fst out
