module Out.Other
( writeSonHier
, writeCCs
, writeLanguageTreeN
, writeSoundChange
) where

import ClassyPrelude

import Data.Language
import Data.Phoneme
import Data.Soundchange

import Out.Lexicon

-- write the sonority hierarchy
writeSonHier :: [Phoneme] -> [[Phoneme]] -> Text
writeSonHier vows cns = "\n\nSonority hierarchy: " ++ "\n/" ++ cListv ++ "/\n/" ++ intercalate "/\n/" cListc ++ "/\n" where
  fListv = map writePhonemeIPA vows
  cListv = intercalate "/, /" fListv
  fListc = map (map writePhonemeIPA) (reverse cns)
  cListc = map (intercalate "/, /") fListc

writeCCs :: [[Phoneme]] -> [[Phoneme]] -> Text
writeCCs onsets codas = "\n\nValid onsets: " ++ "\n/" ++ intercalate "/\n/" oList ++ "/" ++ "\n\nValid codas: " ++ "\n/" ++ intercalate "/\n/" cList ++ "/\n" where
  oList = map (concatMap writePhonemeIPA) onsets
  cList = map (concatMap writePhonemeIPA) codas


-- write language branches into Newick format
writeLanguageTreeN :: LanguageBranch -> Text
writeLanguageTreeN tree = writeLanguageBranchN tree ++ ";"

writeLanguageBranchN :: LanguageBranch -> Text
writeLanguageBranchN (LanguageBranch lang [] n) = fst (getNameMod lang) ++ snd (getNameMod lang) ++ getName lang -- ++ ":" ++ toString (fromIntegral n / 10)
writeLanguageBranchN (LanguageBranch lang branches n) = branchStuff ++ fst (getNameMod lang) ++ snd (getNameMod lang) ++ getName lang where -- ++ ":" ++ toString (fromIntegral n / 10) where
  branchStuff = "(" ++ intercalate "," (map writeLanguageBranchN branches) ++ ")"

-- Write sound changes
writeSoundChange :: [Rule] -> Text
writeSoundChange rs = "\n<br>\nPhonological changes: " ++ "\n" ++ intercalate "\n" (map tshow rs) ++ "\n"
