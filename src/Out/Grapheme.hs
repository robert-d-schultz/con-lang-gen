module Out.Grapheme
( writeCharacter
, writeMorphemeNative
) where

import ClassyPrelude

import Data.Phoneme
import Data.Other
import Data.Inflection

--
writeMorphemeNative :: Morpheme -> [(Phoneme, Text)] -> Text
writeMorphemeNative (Morpheme ps) dict = concatMap (\x -> fromMaybe "<!!MissingChar!!>" (lookup x dict)) ps


-- Grapheme
-- k = size
-- w = stroke width
writeCharacter :: CharPath -> Int -> Int -> Int -> Text
writeCharacter path k w n = "<svg title=\"U" ++ tshow n ++ "\" x=\"" ++ tshow 0 ++ "\" y=\"" ++ tshow 0 ++ "\" width=\"" ++ tshow k ++ "\" height=\"" ++ tshow k ++ "\" viewBox=\"" ++ tshow (0 - 1*w) ++ ", " ++ tshow (0 - 1*w) ++ ", " ++ tshow (k + 2*w) ++ ", " ++ tshow (k + 2*w) ++ "\">" ++ textPath2 ++ "</svg>" where
  textPath = pathToText path
  textPath2 = "<path d=\""++ textPath ++ "\" stroke=\"black\" stroke-width=\"" ++ tshow w ++ "\" fill=\"none\"/>"

pathToText :: CharPath -> Text
pathToText path = unwords $ map tshow path
