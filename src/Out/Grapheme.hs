module Out.Grapheme
( writeCharacter
, writeMorphemeWithAlpha
, writeMorphemeWithSylla
, writeMorphemeWithLogo
) where

import ClassyPrelude

import Data.Phoneme
import Data.Word
import Data.Other
import Data.Inflection

-- Write out using Alphabet
writeMorphemeWithAlpha :: [(Phoneme, Text)] -> Morpheme -> Text
writeMorphemeWithAlpha dict (MorphemeS _ _ sylls) = concatMap (writeSyllableWithAlpha dict) sylls
writeMorphemeWithAlpha dict (MorphemeP _ _ ps)    = concatMap (\x -> fromMaybe "<!!MissingChar!!>" (lookup x dict)) ps
writeMorphemeWithAlpha dict (MorphemeC _ _ rads)  = intercalate "–" $ map (concatMap (\x -> fromMaybe "<!!MissingChar!!>" (lookup x dict))) rads
writeMorphemeWithAlpha dict (MorphemeV _ _ patt)  = intercalate "–" $ map (writeSyllableWithAlpha dict) patt

writeSyllableWithAlpha :: [(Phoneme, Text)] -> Syllable -> Text
writeSyllableWithAlpha dict (Syllable onset nuc coda _ _) = out where
  out = concatMap (\x -> fromMaybe "<!!MissingChar!!>" (lookup x dict)) onset
     ++ fromMaybe "<!!MissingChar!!>" (lookup nuc dict)
     ++ concatMap (\x -> fromMaybe "<!!MissingChar!!>" (lookup x dict)) coda

-- Write out using Syllabary
writeMorphemeWithSylla :: [(Syllable, Text)] -> Morpheme -> Text
writeMorphemeWithSylla dict (MorphemeS _ _ sylls) = concatMap (writeSyllableWithSylla dict) sylls
writeMorphemeWithSylla dict (MorphemeP _ _ ps)    = "" --not sure about this
writeMorphemeWithSylla dict (MorphemeC _ _ rads)  = "" --not sure about this
writeMorphemeWithSylla dict (MorphemeV _ _ patt)  = intercalate "–" $ map (writeSyllableWithSylla dict) patt

writeSyllableWithSylla :: [(Syllable, Text)] -> Syllable -> Text
writeSyllableWithSylla dict syll = fromMaybe "<!!MissingSyll!!>" $ lookup syll dict

-- Write out using Logographic system
writeMorphemeWithLogo :: [(Morpheme, Text)] -> Morpheme -> Text
writeMorphemeWithLogo dict morph = fromMaybe "<!!MissingLogo!!>" $ lookup morph dict

-- Grapheme
-- k = size
-- w = stroke width
writeCharacter :: CharPath -> Int -> Int -> Int -> Text
writeCharacter path k w n = "<svg title=\"U" ++ tshow n ++ "\" x=\"" ++ tshow 0 ++ "\" y=\"" ++ tshow 0 ++ "\" width=\"" ++ tshow k ++ "\" height=\"" ++ tshow k ++ "\" viewBox=\"" ++ tshow (0 - 1*w) ++ ", " ++ tshow (0 - 1*w) ++ ", " ++ tshow (k + 2*w) ++ ", " ++ tshow (k + 2*w) ++ "\">" ++ textPath2 ++ "</svg>" where
  textPath = pathToText path
  textPath2 = "<path d=\""++ textPath ++ "\" stroke=\"black\" stroke-width=\"" ++ tshow w ++ "\" fill=\"none\"/>"

pathToText :: CharPath -> Text
pathToText path = unwords $ map tshow path
