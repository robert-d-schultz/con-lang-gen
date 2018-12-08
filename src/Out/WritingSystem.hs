module Out.WritingSystem
( writeWritingSystem
, makeNativeDict
) where

import ClassyPrelude

import Data.Phoneme
import Data.Word
import Data.Other
import Data.Inflection

import Out.Lexicon
import Out.Grapheme


-- Alphabetic, syllabic, and logographic
writeWritingSystem :: ([(Phoneme, (Int, CharPath))], [(Syllable, (Int, CharPath))], [(Morpheme, (Int, CharPath))]) -> Text
writeWritingSystem (a, s, l) =  "\n<br>\n" ++ aOut ++ sOut ++ lOut where
  aOut | null a = ""
       | otherwise = concatMap writeAChar a
  sOut | null s = ""
       | otherwise = concatMap writeSChar s
  lOut | null l = ""
       | otherwise = concatMap writeLChar l

writeAChar :: (Phoneme, (Int, CharPath)) -> Text
writeAChar (cns@Consonant{}, (n, path)) = "\n<br>\n" ++ writeCharacter path 11 1 n ++ ", /" ++ writePhonemeIPA cns ++ "/"
writeAChar (vow@Vowel{}, (n, path)) = "\n<br>\n" ++ writeCharacter path 11 1 n ++ ", /" ++ writePhonemeIPA vow ++ "/"
writeAChar (diph@Diphthong{}, (n, path)) = "\n<br>\n" ++ writeCharacter path 11 1 n ++ ", /" ++ writePhonemeIPA diph ++ "/"
writeAChar (Blank, _) = ""

writeSChar :: (Syllable, (Int, CharPath)) -> Text
writeSChar (syll, (n, path)) = "\n<br>\n" ++ writeCharacter path 11 1 n ++ ", /" ++ writeSyllableIPA syll ++"/"

writeLChar :: (Morpheme, (Int, CharPath)) -> Text
writeLChar (morph, (n, path)) = "\n<br>\n" ++ writeCharacter path 11 1 n ++ ", \"" ++ out ++ "\"" where
  out = case getMeaning morph of (Meaning _ str) -> str
                                 (InflMeaning _ _ ae) -> tshow ae

makeNativeDict :: [(Phoneme, (Int, CharPath))] -> [(Phoneme, Text)]
makeNativeDict = map (fst &&& (\(_, (n,path))-> writeCharacter path 11 1 n))
