module Out.Other
( parseSonHier
, parseCCs
) where

import Data.List

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
