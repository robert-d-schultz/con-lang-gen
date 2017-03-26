module Out.Other
( parseSonHier
, parseCCs
, parseLanguageTree2
) where

import Data.List

import Data.Other
import Data.Phoneme

import Out.Lexicon

-- Parse the sonority hierarchy
parseSonHier :: [Phoneme] -> [[Phoneme]] -> String
parseSonHier vows cons = "\n\nSonority hierarchy: " ++ "\n/" ++ cListv ++ "/\n/" ++ intercalate "/\n/" cListc ++ "/\n" where
  fListv = map parsePhonemeIPA vows
  cListv = intercalate "/, /" fListv
  fListc = map (map parsePhonemeIPA) cons
  cListc = map (intercalate "/, /") fListc

parseCCs :: [[Phoneme]] -> [[Phoneme]] -> String
parseCCs onsets codas = "\n\nValid onsets: " ++ "\n/" ++ intercalate "/\n/" oList ++ "/" ++ "\n\nValid codas: " ++ "\n/" ++ intercalate "/\n/" cList ++ "/\n" where
  oList = map (concatMap parsePhonemeIPA) onsets
  cList = map (concatMap parsePhonemeIPA) codas


-- Parse language branches into Newick format
parseLanguageTree2 :: LanguageBranch -> String
parseLanguageTree2 tree = parseLanguageBranch2 tree ++ ";"

parseLanguageBranch2 :: LanguageBranch -> String
parseLanguageBranch2 (LanguageBranch lang [] n) = getName lang -- ++ ":" ++ show (fromIntegral n / 10)
parseLanguageBranch2 (LanguageBranch lang branches n) = branchStuff ++ getName lang where -- ++ ":" ++ show (fromIntegral n / 10) where
  branchStuff = "(" ++ intercalate "," (map parseLanguageBranch2 branches) ++ ")"
