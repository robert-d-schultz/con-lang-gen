module Out.Lexicon
( parseDictionary
, parseWordIPA
, parseMorphemeIPA
, parsePhonemeIPA
) where

import Prelude hiding (Word)
import Data.List
import Control.Arrow
import GHC.Exts hiding (Word)

import Data.Phoneme
import Data.Inflection
import Data.Other
import Out.Roman
import Out.Syllable

-- Parse list of words to string
parseDictionary :: [[Phoneme]] -> [((String, LexCat), Word)] -> String
parseDictionary sonHier pairs = "\n" ++ intercalate "\n" (map (parseDictionaryEntry sonHier) (reduceHomophones pairs))

reduceHomophones :: [((String, LexCat), Word)] -> [([(String, LexCat)], Word)]
reduceHomophones pairs = map (second head . unzip) (groupWith snd (sortWith snd pairs))

parseDictionaryEntry :: [[Phoneme]] -> ([(String, LexCat)], Word) -> String
parseDictionaryEntry sonHier (means, wrd) = romanizeWord wrd ++ " (" ++ parseWordIPA sonHier wrd ++ ")" ++ concatMap (\(str, lc) -> "\n\t" ++ parseLC lc ++ " " ++ str) means

parseLC :: LexCat -> String
parseLC lc
  | lc == Verb = "v."
  | lc == Noun = "n."
  | lc == Adj  = "adj."
  | lc == Adv  = "adv."
  | lc == Adpo = "p."

-- Parse Word to string
parseWordIPA :: [[Phoneme]] -> Word -> String
parseWordIPA sonHier word = "/" ++ intercalate "." (map parseSyllableIPA sylls) ++ "/" where
  (SyllWord sylls) = syllabifyWord sonHier word

-- Parse morpheme for use in exponent table
parseMorphemeIPA :: [[Phoneme]] -> Morpheme -> String
parseMorphemeIPA sonHier morph = "/" ++ intercalate "." (map parseSyllableIPA sylls) ++ "/" where
  (SyllWord sylls) = syllabifyMorpheme sonHier morph

-- Parse Syllable to string
parseSyllableIPA :: Syllable -> String
parseSyllableIPA (Syllable onset (Consonant a b c d) coda) = concatMap parsePhonemeIPA onset ++ parsePhonemeIPA (Consonant a b c d) ++ "\809" ++ concatMap parsePhonemeIPA coda
parseSyllableIPA (Syllable onset nucleus coda) = concatMap parsePhonemeIPA onset ++ parsePhonemeIPA nucleus ++ concatMap parsePhonemeIPA coda

-- Parse Phoneme to string
parsePhonemeIPA :: Phoneme -> String
parsePhonemeIPA Blank = ""
parsePhonemeIPA (Consonant _ _ _ ipa) = ipa
parsePhonemeIPA (Vowel _ _ _ _ _ ipa) = ipa
parsePhonemeIPA (Diphthong _ _ _ _ _ _ _ _ ipa) = ipa
