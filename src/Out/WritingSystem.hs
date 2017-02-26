module Out.WritingSystem
( parseWritingSystem
) where

import Data.Phoneme
import Data.Other
import Data.Inflection

import Out.Lexicon

parseWritingSystem :: ([(Phoneme, (Int, String))], [(Syllable, (Int, String))], [(((String, LexCat), Morpheme), (Int, String))]) -> String
parseWritingSystem (a, s, l) = "\n<br>\n" ++ aOut ++ sOut ++ lOut where
  aOut | null a = []
       | otherwise = concatMap parseAChar a
  sOut | null s = []
       | otherwise = concatMap parseSChar s
  lOut | null l = []
       | otherwise = concatMap parseLChar l

parseAChar :: (Phoneme, (Int, String)) -> String
parseAChar (Consonant _ _ _ ipa, (_, svg)) = "\n<br>\n" ++ svg ++ ", /" ++ ipa ++ "/"
parseAChar (Vowel _ _ _ _ _ ipa, (_, svg)) = "\n<br>\n" ++ svg ++ ", /" ++ ipa ++ "/"
parseAChar (Diphthong _ _ _ _ _ _ _ _ ipa, (_, svg)) = "\n<br>\n" ++ svg ++ ", /" ++ ipa ++ "/"


parseSChar :: (Syllable, (Int, String)) -> String
parseSChar (syll, (_, svg)) = "\n<br>\n" ++ svg ++ ", /" ++ parseSyllableIPA syll ++"/"

parseLChar :: (((String, LexCat), Morpheme), (Int, String)) -> String
parseLChar (((str, lc), morph), (_, svg)) = "\n<br>\n" ++ svg ++ ", \"" ++ str ++ "\""
