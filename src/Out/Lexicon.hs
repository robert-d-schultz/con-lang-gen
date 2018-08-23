{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
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
writeRootDictionary :: Language -> [((Text, LexCat), Morpheme)] -> Text
writeRootDictionary lang pairs = "\n" ++ intercalate "\n" (map (writeRootDictionaryEntry lang) (reduceHomophones2 pairs))

reduceHomophones2 :: [((Text, LexCat), Morpheme)] -> [([(Text, LexCat)], Morpheme)]
reduceHomophones2 pairs = map (second (fromMaybe (Morpheme []) . listToMaybe) . unzip) (groupWith snd (sortWith snd pairs))

writeRootDictionaryEntry :: Language -> ([(Text, LexCat)], Morpheme) -> Text
writeRootDictionaryEntry lang (means, morph) = romanizeMorpheme morph ++ " (" ++ writeMorphemeIPA lang morph ++ ")" ++ concatMap (\(str, lc) -> "\n\t" ++ writeLC lc ++ " " ++ str) means

-- write list of words to string
writeDictionary :: Language -> [((Text, LexCat), Word)] -> Text
writeDictionary lang pairs = "\n" ++ intercalate "\n" (map (writeDictionaryEntry lang) (reduceHomophones pairs))

reduceHomophones :: [((Text, LexCat), Word)] -> [([(Text, LexCat)], Word)]
reduceHomophones pairs = map (second (fromMaybe (Word []) . listToMaybe) . unzip) (groupWith snd (sortWith snd pairs))

writeDictionaryEntry :: Language -> ([(Text, LexCat)], Word) -> Text
writeDictionaryEntry lang (means, wrd) = romanizeWord wrd ++ " (" ++ writeWordIPA lang wrd ++ ")" ++ concatMap (\(str, lc) -> "\n\t" ++ writeLC lc ++ " " ++ str) means

writeLC :: LexCat -> Text
writeLC lc
  | lc == Verb = "v."
  | lc == Noun = "n."
  | lc == Adj  = "adj."
  | lc == Adv  = "adv."
  | lc == Adpo = "p."
  | otherwise = ""

-- write Word to string
writeWordIPA :: Language -> Word -> Text
writeWordIPA lang word = fromMaybe "!!Word doesn't syllabize!!" out where
  out = do
    (SyllWord sylls) <- syllabifyWord lang word
    return $ "/" ++ intercalate "." (map writeSyllableIPA sylls) ++ "/"

-- write Morpheme to string (used in exponent table too)
writeMorphemeIPA :: Language -> Morpheme -> Text
writeMorphemeIPA lang m = fromMaybe ("!!Morpheme doesn't syllabize!! /" ++ concatMap writePhonemeIPA (getPhonemes m) ++"/") out where
  out = do
    (SyllWord sylls) <- syllabifyMorpheme lang m
    return $ "/" ++ intercalate "." (map writeSyllableIPA sylls) ++ "/"


-- write Syllable to string
writeSyllableIPA :: Syllable -> Text
writeSyllableIPA (Syllable onset (Consonant a b c) coda) = concatMap writePhonemeIPA onset ++ writePhonemeIPA (Consonant a b c) ++ "\809" ++ concatMap writePhonemeIPA coda
writeSyllableIPA (Syllable onset nucleus coda) = concatMap writePhonemeIPA onset ++ writePhonemeIPA nucleus ++ concatMap writePhonemeIPA coda
