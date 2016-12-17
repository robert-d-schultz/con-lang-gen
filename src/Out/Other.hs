module Out.Other
( parseSonHier
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
