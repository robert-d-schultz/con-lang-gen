{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
module Out.WritingSystem
( writeWritingSystem
, writeCharacter
) where

import ClassyPrelude

import Data.Phoneme
import Data.Other
import Data.Inflection

import Out.Lexicon

writeWritingSystem :: ([(Phoneme, (Int, [(Text,[(Int,Int)])]))], [(Syllable, (Int, [(Text,[(Int,Int)])]))], [(((Text, LexCat), Morpheme), (Int, [(Text,[(Int,Int)])]))]) -> Text
writeWritingSystem (a, s, l) =  "\n<br>\n" ++ aOut ++ sOut ++ lOut where
  aOut | null a = ""
       | otherwise = concatMap writeAChar a
  sOut | null s = ""
       | otherwise = concatMap writeSChar s
  lOut | null l = ""
       | otherwise = concatMap writeLChar l

writeAChar :: (Phoneme, (Int, [(Text,[(Int,Int)])])) -> Text
writeAChar (Consonant p m h, (n, path)) = "\n<br>\n" ++ writeCharacter path 11 1 n ++ ", /" ++ writePhonemeIPA (Consonant p m h) ++ "/"
writeAChar (Vowel h b r l t, (n, path)) = "\n<br>\n" ++ writeCharacter path 11 1 n ++ ", /" ++ writePhonemeIPA (Vowel h b r l t) ++ "/"
writeAChar (Diphthong h1 b1 r1 h2 b2 r2 l t, (n, path)) = "\n<br>\n" ++ writeCharacter path 11 1 n ++ ", /" ++ writePhonemeIPA (Diphthong h1 b1 r1 h2 b2 r2 l t) ++ "/"
writeAChar (Blank, _) = ""

writeSChar :: (Syllable, (Int, [(Text,[(Int,Int)])])) -> Text
writeSChar (syll, (n, path)) = "\n<br>\n" ++ writeCharacter path 11 1 n ++ ", /" ++ writeSyllableIPA syll ++"/"

writeLChar :: (((Text, LexCat), Morpheme), (Int, [(Text,[(Int,Int)])])) -> Text
writeLChar (((str, _), _), (n, path)) = "\n<br>\n" ++ writeCharacter path 11 1 n ++ ", \"" ++ str ++ "\""

-- Grapheme
writeCharacter :: [(Text,[(Int,Int)])] -> Int -> Int -> Int -> Text
writeCharacter path k w n = "<svg title=\"U" ++ tshow n ++ "\" x=\"0\" y=\"0\" width=\"" ++ tshow (k + w) ++ "\" height=\"" ++ tshow (k + w) ++ "\" viewBox=\"0, 0, " ++ tshow (k + w) ++ ", " ++ tshow (k + w) ++ "\">" ++ textPath2 ++ "</svg>" where
  textPath = pathToText path
  textPath2 = "<path d=\""++ textPath ++ "\" stroke=\"black\" stroke-width=\"" ++ tshow w ++ "\" fill=\"none\"/>"

pathToText :: [(Text,[(Int,Int)])] -> Text
pathToText old = concat out where
  out = map (\(str, ints) -> str ++ concatMap (\(x,y) -> (if x >= 0 then tshow x ++ " " else "") ++ (if y >= 0 then tshow y ++ " " else "")) ints) old
