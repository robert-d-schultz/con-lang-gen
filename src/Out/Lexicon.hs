{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Out.Lexicon
( writeDictionary
, writeWordIPA
, writeMorphemeIPA
, writePhonemeIPA
, writeRootDictionary
, writeSyllableIPA
, writeLC
) where

import ClassyPrelude hiding (Word)
import GHC.Exts (groupWith)

import Data.Phoneme
import Data.Inflection
import Data.Other

import Out.Roman
import Out.Syllable
import Out.IPA

-- write list of roots to string
writeRootDictionary :: [[Phoneme]] -> [((Text, LexCat), Morpheme)] -> Text
writeRootDictionary sonHier pairs = "\n" ++ intercalate "\n" (map (writeRootDictionaryEntry sonHier) (reduceHomophones2 pairs))

reduceHomophones2 :: [((Text, LexCat), Morpheme)] -> [([(Text, LexCat)], Morpheme)]
reduceHomophones2 pairs = map (second (fromMaybe (Morpheme []) . listToMaybe) . unzip) (groupWith snd (sortWith snd pairs))

writeRootDictionaryEntry :: [[Phoneme]] -> ([(Text, LexCat)], Morpheme) -> Text
writeRootDictionaryEntry sonHier (means, morph) = romanizeMorpheme morph ++ " (" ++ writeMorphemeIPA sonHier morph ++ ")" ++ concatMap (\(str, lc) -> "\n\t" ++ writeLC lc ++ " " ++ str) means

-- write list of words to string
writeDictionary :: [[Phoneme]] -> [((Text, LexCat), Word)] -> Text
writeDictionary sonHier pairs = "\n" ++ intercalate "\n" (map (writeDictionaryEntry sonHier) (reduceHomophones pairs))

reduceHomophones :: [((Text, LexCat), Word)] -> [([(Text, LexCat)], Word)]
reduceHomophones pairs = map (second (fromMaybe (Word []) . listToMaybe) . unzip) (groupWith snd (sortWith snd pairs))

writeDictionaryEntry :: [[Phoneme]] -> ([(Text, LexCat)], Word) -> Text
writeDictionaryEntry sonHier (means, wrd) = romanizeWord wrd ++ " (" ++ writeWordIPA sonHier wrd ++ ")" ++ concatMap (\(str, lc) -> "\n\t" ++ writeLC lc ++ " " ++ str) means

writeLC :: LexCat -> Text
writeLC lc
  | lc == Verb = "v."
  | lc == Noun = "n."
  | lc == Adj  = "adj."
  | lc == Adv  = "adv."
  | lc == Adpo = "p."

-- write Word to string
writeWordIPA :: [[Phoneme]] -> Word -> Text
writeWordIPA sonHier word = "/" ++ intercalate "." (map writeSyllableIPA sylls) ++ "/" where
  (SyllWord sylls) = syllabifyWord sonHier word

-- write Morpheme to string (used in exponent table too)
writeMorphemeIPA :: [[Phoneme]] -> Morpheme -> Text
writeMorphemeIPA sonHier morph = "/" ++ intercalate "." (map writeSyllableIPA sylls) ++ "/" where
  (SyllWord sylls) = syllabifyMorpheme sonHier morph

-- write Syllable to string
writeSyllableIPA :: Syllable -> Text
writeSyllableIPA (Syllable onset (Consonant a b c) coda) = concatMap writePhonemeIPA onset ++ writePhonemeIPA (Consonant a b c) ++ "\809" ++ concatMap writePhonemeIPA coda
writeSyllableIPA (Syllable onset nucleus coda) = concatMap writePhonemeIPA onset ++ writePhonemeIPA nucleus ++ concatMap writePhonemeIPA coda
