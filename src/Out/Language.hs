module Out.Language
( writeLanguageTree
, writeLanguage
) where

import ClassyPrelude hiding (writeFile)
import Data.Text.IO (writeFile)

import Data.RVar
import System.Directory
import Control.Monad as M (replicateM)

import Gen.Phonotactics
import Gen.ParseTree

import Out.Other
import Out.Phonology
import Out.Inflection
import Out.Lexicon
import Out.Sentence
import Out.Grammar
import Out.WritingSystem
import Out.Grapheme

import Data.Language


-- outputs stuff for each language family
writeLanguageTree :: Int -> LanguageBranch -> IO()
writeLanguageTree seed tree = do
  -- Create language family main folder (named afer the seed)
  let dir = "out/" ++ tshow seed
  createDirectory (unpack dir)

  let treeMap = writeLanguageTreeN tree
  writeFile (unpack (dir ++ "/tree_map.html")) treeMap
  writeLanguageBranch dir tree

writeLanguageBranch :: Text -> LanguageBranch -> IO()
-- writeLanguageBranch dir (LanguageBranch lang [] _) = writeLanguage dir lang
writeLanguageBranch dir (LanguageBranch lang branches _) = do
  let name = fst (getNameMod lang) ++ snd (getNameMod lang) ++ getName lang
  let dir_ = dir ++ "/" ++ name
  dir__ <- makeDirectory dir_ 0
  createDirectory (unpack dir__)

  writeLanguage dir__ lang
  mapM_ (writeLanguageBranch dir__) branches

-- outputs stuff for each language
writeLanguage :: Text -> Language -> IO()
writeLanguage dir lang = do
  let (_, _, phonations, _) = getCMap lang
  let (_, _, rounds, _) = getVMap lang
  let inventoryD = getDInv lang
  let tones = getTones lang
  let onsetCCs = getOnsetCCs lang
  let codaCCs = getCodaCCs lang
  let scheme = getSonHier lang
  let grammar = getGrammar lang
  let inflSys = getInflMap lang
  let systems = getManSyss lang
  let roots = getRoots lang
  let (alph, syll, logo) = getWriting lang
  let name = fst (getNameMod lang) ++ snd (getNameMod lang) ++ getName lang

  let inventoryC = getCInv lang
  let inventoryV = getVInv lang

  let sonHier = makeSonHier inventoryC scheme

  let rules = getRules lang

  -- Writing
  -- let characterSVGs = map snd alph ++ map snd syll ++ map snd logo

  let ndict = []
  -- let ndict = makeNativeDict alph

  -- Parse trees
  ptExamples <- sampleRVar $ M.replicateM 5 (makeParseTree roots inflSys)

  -- Make directory


  -- Write to file a bunch of stuff
  writeFile (unpack dir ++ "/phonology.html") $ "Phonology"
                                 ++ writeConPhonemeInventory inventoryC
                                 ++ writePhonationInventory phonations
                                 ++ writeVowPhonemeInventory inventoryV
                                 ++ writeRoundednessInventory rounds
                                 ++ writeDiphPhonemeInventory inventoryD
                                 ++ writeToneInventory tones

  writeFile (unpack dir ++ "/phonotactics.html") $ "Phonotactics"
                                 ++ writeSonHier (inventoryV ++ inventoryD) sonHier
                                 ++ writeCCs onsetCCs codaCCs

  writeFile (unpack dir ++ "/inflection.html") $ "Inflection"
                                 ++ writeLCInflection inflSys
                                 ++ writeLexicalSystems inflSys lang systems

  writeFile (unpack dir ++ "/lexicon.html") $ "Lexicon"
                              -- ++ writeDictionary sonHier dict
                                 ++ writeRootDictionary lang ndict roots

  writeFile (unpack dir ++ "/grammar.html") $ "Grammar"
                                 ++ writeGrammar grammar
                                 ++ "Examples"
                                 ++ concatMap (writeParseTree lang roots systems) ptExamples

  writeFile (unpack dir ++ "/writing system.html") $ "Writing System"
                                 ++ writeWritingSystem (alph, syll, logo)

  writeFile (unpack dir ++ "/language changes.html") $ "Language Changes"
                                ++ writeSoundChange rules


  -- createDirectory $ dir + "characters"
  -- let t = map (\(num, str) -> (dir ++ "characters/U+" ++ tshow num ++ ".svg", str)) characterSVGs
  -- forM_ t $ uncurry writeFile

-- Used when multiple languages share the same name
makeDirectory :: Text -> Int -> IO Text
makeDirectory dir 0 = do
  exist <- doesPathExist (unpack dir)
  if exist then makeDirectory dir 1 else return dir
makeDirectory dir count = do
  let dir_ = dir ++ tshow count
  exist <- doesPathExist (unpack dir_)
  if exist then makeDirectory dir (count + 1) else return dir_
