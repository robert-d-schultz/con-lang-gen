{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Out.Other
( writeSonHier
, writeCCs
, writeLanguageTreeN
) where

import ClassyPrelude

import Data.Other
import Data.Phoneme

import Out.Lexicon

-- write the sonority hierarchy
writeSonHier :: [Phoneme] -> [[Phoneme]] -> Text
writeSonHier vows cons = "\n\nSonority hierarchy: " ++ "\n/" ++ cListv ++ "/\n/" ++ intercalate "/\n/" cListc ++ "/\n" where
  fListv = map writePhonemeIPA vows
  cListv = intercalate "/, /" fListv
  fListc = map (map writePhonemeIPA) cons
  cListc = map (intercalate "/, /") fListc

writeCCs :: [[Phoneme]] -> [[Phoneme]] -> Text
writeCCs onsets codas = "\n\nValid onsets: " ++ "\n/" ++ intercalate "/\n/" oList ++ "/" ++ "\n\nValid codas: " ++ "\n/" ++ intercalate "/\n/" cList ++ "/\n" where
  oList = map (concatMap writePhonemeIPA) onsets
  cList = map (concatMap writePhonemeIPA) codas


-- write language branches into Newick format
writeLanguageTreeN :: LanguageBranch -> Text
writeLanguageTreeN tree = writeLanguageBranchN tree ++ ";"

writeLanguageBranchN :: LanguageBranch -> Text
writeLanguageBranchN (LanguageBranch lang [] n) = getName lang -- ++ ":" ++ show (fromIntegral n / 10)
writeLanguageBranchN (LanguageBranch lang branches n) = branchStuff ++ getName lang where -- ++ ":" ++ show (fromIntegral n / 10) where
  branchStuff = "(" ++ intercalate "," (map writeLanguageBranchN branches) ++ ")"
