module Out.WritingSystem
( parseWritingSystem
, parseCharacter
) where

import Data.Phoneme
import Data.Other
import Data.Inflection

import Out.Lexicon

parseWritingSystem :: ([(Phoneme, (Int, [(String,[(Int,Int)])]))], [(Syllable, (Int, [(String,[(Int,Int)])]))], [(((String, LexCat), Morpheme), (Int, [(String,[(Int,Int)])]))]) -> String
parseWritingSystem (a, s, l) = "\n<br>\n" ++ aOut ++ sOut ++ lOut where
  aOut | null a = []
       | otherwise = concatMap parseAChar a
  sOut | null s = []
       | otherwise = concatMap parseSChar s
  lOut | null l = []
       | otherwise = concatMap parseLChar l

parseAChar :: (Phoneme, (Int, [(String,[(Int,Int)])])) -> String
parseAChar (Consonant _ _ _ ipa, (m, path)) = "\n<br>\n" ++ parseCharacter path 11 1 m ++ ", /" ++ ipa ++ "/"
parseAChar (Vowel _ _ _ _ _ ipa, (m, path)) = "\n<br>\n" ++ parseCharacter path 11 1 m ++ ", /" ++ ipa ++ "/"
parseAChar (Diphthong _ _ _ _ _ _ _ _ ipa, (m, path)) = "\n<br>\n" ++ parseCharacter path 11 1 m ++ ", /" ++ ipa ++ "/"

parseSChar :: (Syllable, (Int, [(String,[(Int,Int)])])) -> String
parseSChar (syll, (m, path)) = "\n<br>\n" ++ parseCharacter path 11 1 m ++ ", /" ++ parseSyllableIPA syll ++"/"

parseLChar :: (((String, LexCat), Morpheme), (Int, [(String,[(Int,Int)])])) -> String
parseLChar (((str, lc), morph), (m, path)) = "\n<br>\n" ++ parseCharacter path 11 1 m ++ ", \"" ++ str ++ "\""

-- Grapheme
parseCharacter :: [(String,[(Int,Int)])] -> Int -> Int -> Int -> String
parseCharacter path n w m = "<svg title=\"U" ++ show m ++ "\" x=\"0\" y=\"0\" width=\"" ++ show (n + w) ++ "\" height=\"" ++ show (n + w) ++ "\" viewBox=\"0, 0, " ++ show (n + w) ++ ", " ++ show (n + w) ++ "\">" ++ textPath2 ++ "</svg>" where
  textPath = pathToText path
  textPath2 = "<path d=\""++ textPath ++ "\" stroke=\"black\" stroke-width=\"" ++ show w ++ "\" fill=\"none\"/>"

pathToText :: [(String,[(Int,Int)])] -> String
pathToText old = concat out where
  out = map (\(str, ints) -> str ++ concatMap (\(x,y) -> (if x >= 0 then show x ++ " " else "") ++ (if y >= 0 then show y ++ " " else "")) ints) old
