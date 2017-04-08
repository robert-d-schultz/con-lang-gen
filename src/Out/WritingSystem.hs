module Out.WritingSystem
( parseWritingSystem
, parseCharacter
) where

import Data.Phoneme
import Data.Other
import Data.Inflection

import Out.Lexicon
import Out.IPA

parseWritingSystem :: ([(Phoneme, (Int, [(String,[(Int,Int)])]))], [(Syllable, (Int, [(String,[(Int,Int)])]))], [(((String, LexCat), Morpheme), (Int, [(String,[(Int,Int)])]))]) -> String
parseWritingSystem (a, s, l) = "\n<br>\n" ++ aOut ++ sOut ++ lOut where
  aOut | null a = []
       | otherwise = concatMap parseAChar a
  sOut | null s = []
       | otherwise = concatMap parseSChar s
  lOut | null l = []
       | otherwise = concatMap parseLChar l

parseAChar :: (Phoneme, (Int, [(String,[(Int,Int)])])) -> String
parseAChar (Consonant p m h, (n, path)) = "\n<br>\n" ++ parseCharacter path 11 1 n ++ ", /" ++ parsePhonemeIPA (Consonant p m h) ++ "/"
parseAChar (Vowel h b r l t, (n, path)) = "\n<br>\n" ++ parseCharacter path 11 1 n ++ ", /" ++ parsePhonemeIPA (Vowel h b r l t) ++ "/"
parseAChar (Diphthong h1 b1 r1 h2 b2 r2 l t, (n, path)) = "\n<br>\n" ++ parseCharacter path 11 1 n ++ ", /" ++ parsePhonemeIPA (Diphthong h1 b1 r1 h2 b2 r2 l t) ++ "/"

parseSChar :: (Syllable, (Int, [(String,[(Int,Int)])])) -> String
parseSChar (syll, (n, path)) = "\n<br>\n" ++ parseCharacter path 11 1 n ++ ", /" ++ parseSyllableIPA syll ++"/"

parseLChar :: (((String, LexCat), Morpheme), (Int, [(String,[(Int,Int)])])) -> String
parseLChar (((str, lc), morph), (n, path)) = "\n<br>\n" ++ parseCharacter path 11 1 n ++ ", \"" ++ str ++ "\""

-- Grapheme
parseCharacter :: [(String,[(Int,Int)])] -> Int -> Int -> Int -> String
parseCharacter path k w n = "<svg title=\"U" ++ show n ++ "\" x=\"0\" y=\"0\" width=\"" ++ show (k + w) ++ "\" height=\"" ++ show (k + w) ++ "\" viewBox=\"0, 0, " ++ show (k + w) ++ ", " ++ show (k + w) ++ "\">" ++ textPath2 ++ "</svg>" where
  textPath = pathToText path
  textPath2 = "<path d=\""++ textPath ++ "\" stroke=\"black\" stroke-width=\"" ++ show w ++ "\" fill=\"none\"/>"

pathToText :: [(String,[(Int,Int)])] -> String
pathToText old = concat out where
  out = map (\(str, ints) -> str ++ concatMap (\(x,y) -> (if x >= 0 then show x ++ " " else "") ++ (if y >= 0 then show y ++ " " else "")) ints) old
