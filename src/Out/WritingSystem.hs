{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Out.WritingSystem
( parseWritingSystem
, parseCharacter
) where

import ClassyPrelude

import Data.Phoneme
import Data.Other
import Data.Inflection

import Out.Lexicon
import Out.IPA

parseWritingSystem :: ([(Phoneme, (Int, [(Text,[(Int,Int)])]))], [(Syllable, (Int, [(Text,[(Int,Int)])]))], [(((Text, LexCat), Morpheme), (Int, [(Text,[(Int,Int)])]))]) -> Text
parseWritingSystem (a, s, l) =  "\n<br>\n" ++ aOut ++ sOut ++ lOut where
  aOut | null a = ""
       | otherwise = concatMap parseAChar a
  sOut | null s = ""
       | otherwise = concatMap parseSChar s
  lOut | null l = ""
       | otherwise = concatMap parseLChar l

parseAChar :: (Phoneme, (Int, [(Text,[(Int,Int)])])) -> Text
parseAChar (Consonant p m h, (n, path)) = "\n<br>\n" ++ parseCharacter path 11 1 n ++ ", /" ++ parsePhonemeIPA (Consonant p m h) ++ "/"
parseAChar (Vowel h b r l t, (n, path)) = "\n<br>\n" ++ parseCharacter path 11 1 n ++ ", /" ++ parsePhonemeIPA (Vowel h b r l t) ++ "/"
parseAChar (Diphthong h1 b1 r1 h2 b2 r2 l t, (n, path)) = "\n<br>\n" ++ parseCharacter path 11 1 n ++ ", /" ++ parsePhonemeIPA (Diphthong h1 b1 r1 h2 b2 r2 l t) ++ "/"

parseSChar :: (Syllable, (Int, [(Text,[(Int,Int)])])) -> Text
parseSChar (syll, (n, path)) = "\n<br>\n" ++ parseCharacter path 11 1 n ++ ", /" ++ parseSyllableIPA syll ++"/"

parseLChar :: (((Text, LexCat), Morpheme), (Int, [(Text,[(Int,Int)])])) -> Text
parseLChar (((str, lc), morph), (n, path)) = "\n<br>\n" ++ parseCharacter path 11 1 n ++ ", \"" ++ str ++ "\""

-- Grapheme
parseCharacter :: [(Text,[(Int,Int)])] -> Int -> Int -> Int -> Text
parseCharacter path k w n = "<svg title=\"U" ++ tshow n ++ "\" x=\"0\" y=\"0\" width=\"" ++ tshow (k + w) ++ "\" height=\"" ++ tshow (k + w) ++ "\" viewBox=\"0, 0, " ++ tshow (k + w) ++ ", " ++ tshow (k + w) ++ "\">" ++ textPath2 ++ "</svg>" where
  textPath = pathToText path
  textPath2 = "<path d=\""++ textPath ++ "\" stroke=\"black\" stroke-width=\"" ++ tshow w ++ "\" fill=\"none\"/>"

pathToText :: [(Text,[(Int,Int)])] -> Text
pathToText old = concat out where
  out = map (\(str, ints) -> str ++ concatMap (\(x,y) -> (if x >= 0 then tshow x ++ " " else "") ++ (if y >= 0 then tshow y ++ " " else "")) ints) old
