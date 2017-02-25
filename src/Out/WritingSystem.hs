module Out.WritingSystem
( parseWritingSystem
) where

import Data.Phoneme
import Data.Other
import Data.Inflection

import Out.Lexicon

parseWritingSystem :: ([(Phoneme, (Int, String))], [(Syllable, (Int, String))], [(((String, LexCat), Morpheme), (Int, String))]) -> String
parseWritingSystem (a, s, l) = "\n" ++ aOut ++ sOut ++ lOut where
  aOut | null a = []
       | otherwise = concatMap parseAChar a
  sOut | null s = []
       | otherwise = concatMap parseSChar s
  lOut | null l = []
       | otherwise = concatMap parseLChar l

parseAChar :: (Phoneme, (Int, String)) -> String
parseAChar (Consonant _ _ _ ipa, (_, svg)) = "\n/" ++ ipa ++ "/\n" ++ svg
parseAChar (Vowel _ _ _ _ _ ipa, (_, svg)) = "\n/" ++ ipa ++ "/\n" ++ svg
parseAChar (Diphthong _ _ _ _ _ _ _ _ ipa, (_, svg)) = "\n/" ++ ipa ++ "/\n" ++ svg


parseSChar :: (Syllable, (Int, String)) -> String
parseSChar (syll, (_, svg)) = "\n/" ++ parseSyllableIPA syll ++ "/\n" ++ svg

parseLChar :: (((String, LexCat), Morpheme), (Int, String)) -> String
parseLChar (((str, lc), morph), (_, svg)) = "\n\"" ++ str ++ "\"\n " ++ svg
