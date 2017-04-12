{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Out.Lexicon
( parseDictionary
, parseWordIPA
, parseMorphemeIPA
, parsePhonemeIPA
, parseRootDictionary
, parseSyllableIPA
, parseLC
) where

import ClassyPrelude hiding (Word)
import GHC.Exts (groupWith)

import Data.Phoneme
import Data.Inflection
import Data.Other

import Out.Roman
import Out.Syllable
import Out.IPA

-- Parse list of roots to string
parseRootDictionary :: [[Phoneme]] -> [((Text, LexCat), Morpheme)] -> Text
parseRootDictionary sonHier pairs = "\n" ++ intercalate "\n" (map (parseRootDictionaryEntry sonHier) (reduceHomophones2 pairs))

reduceHomophones2 :: [((Text, LexCat), Morpheme)] -> [([(Text, LexCat)], Morpheme)]
reduceHomophones2 pairs = map (second (fromMaybe (Morpheme []) . listToMaybe) . unzip) (groupWith snd (sortWith snd pairs))

parseRootDictionaryEntry :: [[Phoneme]] -> ([(Text, LexCat)], Morpheme) -> Text
parseRootDictionaryEntry sonHier (means, morph) = romanizeMorpheme morph ++ " (" ++ parseMorphemeIPA sonHier morph ++ ")" ++ concatMap (\(str, lc) -> "\n\t" ++ parseLC lc ++ " " ++ str) means

-- Parse list of words to string
parseDictionary :: [[Phoneme]] -> [((Text, LexCat), Word)] -> Text
parseDictionary sonHier pairs = "\n" ++ intercalate "\n" (map (parseDictionaryEntry sonHier) (reduceHomophones pairs))

reduceHomophones :: [((Text, LexCat), Word)] -> [([(Text, LexCat)], Word)]
reduceHomophones pairs = map (second (fromMaybe (Word []) . listToMaybe) . unzip) (groupWith snd (sortWith snd pairs))

parseDictionaryEntry :: [[Phoneme]] -> ([(Text, LexCat)], Word) -> Text
parseDictionaryEntry sonHier (means, wrd) = romanizeWord wrd ++ " (" ++ parseWordIPA sonHier wrd ++ ")" ++ concatMap (\(str, lc) -> "\n\t" ++ parseLC lc ++ " " ++ str) means

parseLC :: LexCat -> Text
parseLC lc
  | lc == Verb = "v."
  | lc == Noun = "n."
  | lc == Adj  = "adj."
  | lc == Adv  = "adv."
  | lc == Adpo = "p."

-- Parse Word to string
parseWordIPA :: [[Phoneme]] -> Word -> Text
parseWordIPA sonHier word = "/" ++ intercalate "." (map parseSyllableIPA sylls) ++ "/" where
  (SyllWord sylls) = syllabifyWord sonHier word

-- Parse Morpheme to string (used in exponent table too)
parseMorphemeIPA :: [[Phoneme]] -> Morpheme -> Text
parseMorphemeIPA sonHier morph = "/" ++ intercalate "." (map parseSyllableIPA sylls) ++ "/" where
  (SyllWord sylls) = syllabifyMorpheme sonHier morph

-- Parse Syllable to string
parseSyllableIPA :: Syllable -> Text
parseSyllableIPA (Syllable onset (Consonant a b c) coda) = concatMap parsePhonemeIPA onset ++ parsePhonemeIPA (Consonant a b c) ++ "\809" ++ concatMap parsePhonemeIPA coda
parseSyllableIPA (Syllable onset nucleus coda) = concatMap parsePhonemeIPA onset ++ parsePhonemeIPA nucleus ++ concatMap parsePhonemeIPA coda
