{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
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

import Data.Other


-- outputs stuff for each language family
writeLanguageTree :: Int -> LanguageBranch -> IO()
writeLanguageTree seed tree = do
  let dir = "out/" ++ tshow seed ++ "/"
  createDirectory (unpack dir)

  let treeMap = writeLanguageTreeN tree
  writeFile (unpack (dir ++ "tree_map.html")) treeMap
  writeLanguageBranch seed tree

writeLanguageBranch :: Int -> LanguageBranch -> IO()
writeLanguageBranch seed (LanguageBranch lang [] _) = writeLanguage lang seed
writeLanguageBranch seed (LanguageBranch lang branches _) = writeLanguage lang seed <* mapM_ (writeLanguageBranch seed) branches

-- outputs stuff for each language
writeLanguage :: Language -> Int -> IO()
writeLanguage lang seed = do
  let (_, _, phonations) = getCMap lang
  let (_, _, rounds, _, tones) = getVMap lang
  let inventoryD = getDInv lang
  let onsetCCs = getOnsetCCs lang
  let codaCCs = getCodaCCs lang
  let scheme = getSonHier lang
  let grammar = getGrammar lang
  let inflSys = getInflMap lang
  let systems = getManSyss lang
  let roots = getRoots lang
  let (alph, syll, logo) = getWriting lang
  let name = getName lang

  let inventoryC = getCInv lang
  let inventoryV = getVInv lang

  let sonHier = makeSonHier inventoryC scheme

  -- writing
  let characterSVGs = map snd alph ++ map snd syll ++ map snd logo

  -- parse trees
  ptExamples <- sampleRVar $ M.replicateM 5 (makeParseTree roots inflSys)

  -- make directory
  dir <- makeDirectory seed name 0
  createDirectory (unpack dir)

  -- write to file a bunch of stuff
  writeFile (unpack dir ++ "phonology.html") $ "Phonology"
                                 ++ writeConPhonemeInventory inventoryC
                                 ++ writePhonationInventory phonations
                                 ++ writeVowPhonemeInventory inventoryV
                                 ++ writeRoundednessInventory rounds
                                 ++ writeDiphPhonemeInventory inventoryD
                                 ++ writeToneInventory tones

  writeFile (unpack dir ++ "phonotactics.html") $ "Phonotactics"
                                 ++ writeSonHier (inventoryV ++ inventoryD) sonHier
                                 ++ writeCCs onsetCCs codaCCs

  writeFile (unpack dir ++ "inflection.html") $ "Inflection"
                                 ++ writeLCInflection inflSys
                                 ++ writeLexicalSystems inflSys lang systems

  writeFile (unpack dir ++ "lexicon.html") $ "Lexicon"
                              -- ++ writeDictionary sonHier dict
                                 ++ writeRootDictionary lang roots

  writeFile (unpack dir ++ "grammar.html") $ "Grammar"
                                 ++ writeGrammar grammar
                                 ++ "Examples"
                                 ++ concatMap (writeParseTree lang roots systems) ptExamples

  writeFile (unpack dir ++ "writing system.html") $ "Writing System"
                                 ++ writeWritingSystem (alph, syll, logo)

  -- createDirectory $ dir + "characters"
  -- let t = map (\(num, str) -> (dir ++ "characters/U+" ++ tshow num ++ ".svg", str)) characterSVGs
  -- forM_ t $ uncurry writeFile

-- used when multiple languages share the same name
makeDirectory :: Int -> Text -> Int -> IO Text
makeDirectory seed name 0 = do
  let dir = "out/" ++ tshow seed ++ "/" ++ name ++ "/"
  exist <- doesPathExist (unpack dir)
  if exist then makeDirectory seed name 1 else return dir
makeDirectory seed name count = do
  let dir = "out/" ++ tshow seed ++ "/" ++ name ++ tshow count ++ "/"
  exist <- doesPathExist (unpack dir)
  if exist then makeDirectory seed name (count + 1) else return dir
