module Out.Lexicon
( writeDictionary
, writeWordIPA
, writeMorphemeIPA
, writePhonemeIPA
, writeRootDictionary
, writeSyllWordIPA
, writeSyllableIPA
, writeLC
) where

import ClassyPrelude
import GHC.Exts (groupWith)

import Data.Phoneme
import Data.Inflection
import Data.Language

import Out.Roman
import Out.Syllable
import Out.IPA
import Out.Grapheme

-- write list of roots to string
writeRootDictionary :: Language -> [(Phoneme, Text)] -> [((Text, LexCat), SyllWord)] -> Text
writeRootDictionary lang ndict pairs = "\n" ++ intercalate "\n" (map (writeRootDictionaryEntry lang ndict) (reduceHomophones2 pairs))

reduceHomophones2 :: [((Text, LexCat), SyllWord)] -> [([(Text, LexCat)], SyllWord)]
reduceHomophones2 pairs = map (second (fromMaybe (SyllWord []) . listToMaybe) . unzip) (groupWith snd (sortWith snd pairs))

writeRootDictionaryEntry :: Language -> [(Phoneme, Text)] -> ([(Text, LexCat)], SyllWord) -> Text
writeRootDictionaryEntry lang ndict (means, syllword) = "<br>\n"
                                          --  ++ writeMorphemeNative morph ndict
                                            ++ " <i>" ++ romanizeSyllWord syllword ++ "</i>"
                                            ++ " (" ++ writeSyllWordIPA syllword ++ ")"
                                            ++ concatMap (\(str, lc) -> "\n<br>\t" ++ writeLC lc ++ " " ++ str) means
-- write list of words to string
writeDictionary :: Language -> [((Text, LexCat), SyllWord)] -> Text
writeDictionary lang pairs = "\n" ++ intercalate "\n" (map (writeDictionaryEntry lang) (reduceHomophones pairs))

reduceHomophones :: [((Text, LexCat), SyllWord)] -> [([(Text, LexCat)], SyllWord)]
reduceHomophones pairs = map (second (fromMaybe (SyllWord []) . listToMaybe) . unzip) (groupWith snd (sortWith snd pairs))

writeDictionaryEntry :: Language -> ([(Text, LexCat)], SyllWord) -> Text
writeDictionaryEntry lang (means, wrd) = romanizeSyllWord wrd ++ " (" ++ writeSyllWordIPA wrd ++ ")" ++ concatMap (\(str, lc) -> "\n\t" ++ writeLC lc ++ " " ++ str) means

writeLC :: LexCat -> Text
writeLC lc
  | lc == Verb = "v."
  | lc == Noun = "n."
  | lc == Adj  = "adj."
  | lc == Adv  = "adv."
  | lc == Adpo = "p."
  | otherwise = ""

-- write Word to string
writeWordIPA :: Language -> MorphWord -> Text
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

-- write Syllable-Word to string
writeSyllWordIPA :: SyllWord -> Text
writeSyllWordIPA (SyllWord sylls) = out where
  ipa = concatMap writeSyllableIPA sylls
  out
    | singleton (unsafeLast ipa) == ("." :: Text) = unsafeInit ipa
    | otherwise = ipa

-- write Syllable to string
writeSyllableIPA :: Syllable -> Text
writeSyllableIPA (Syllable onset nuc@Consonant{} coda tone stress) = concatMap writePhonemeIPA onset
                                                                  ++ writePhonemeIPA nuc
                                                                  ++ "\809"
                                                                  ++ concatMap writePhonemeIPA coda
                                                                  ++ writeToneLetterIPA tone
                                                                  ++ writeStressMarkIPA stress
writeSyllableIPA (Syllable onset nucleus coda tone stress) = concatMap writePhonemeIPA onset
                                                          ++ writePhonemeIPA nucleus
                                                          ++ concatMap writePhonemeIPA coda
                                                          ++ writeToneLetterIPA tone
                                                          ++ writeStressMarkIPA stress
