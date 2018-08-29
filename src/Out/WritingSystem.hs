module Out.WritingSystem
( writeWritingSystem
, makeNativeDict
) where

import ClassyPrelude

import Data.Phoneme
import Data.Other
import Data.Inflection

import Out.Lexicon
import Out.Grapheme

writeWritingSystem :: ([(Phoneme, (Int, CharPath))], [(Syllable, (Int, CharPath))], [(((Text, LexCat), SyllWord), (Int, CharPath))]) -> Text
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

writeLChar :: (((Text, LexCat), SyllWord), (Int, CharPath)) -> Text
writeLChar (((str, _), _), (n, path)) = "\n<br>\n" ++ writeCharacter path 11 1 n ++ ", \"" ++ str ++ "\""

makeNativeDict :: [(Phoneme, (Int, CharPath))] -> [(Phoneme, Text)]
makeNativeDict = map (fst &&& (\(_, (n,path))-> writeCharacter path 11 1 n))
