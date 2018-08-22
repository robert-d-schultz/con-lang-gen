module Morph.PhonologyV
( morphPhonologyV
) where

import Data.RVar
import Data.Random.Extras
import Control.Monad
import Data.List
import Data.Maybe
import Data.Ord

import HelperFunctions

import Data.Phoneme
import Data.Inflection
import Data.Other

import Gen.Phonology
import Gen.Phonotactics

import Constants

import Debug.Trace

-- there should be two versions of this
-- one for vowel shifts, where vowels are shifted in a closed loop
-- and another where vowels are inserted and deleted
morphPhonologyV :: Language -> RVar Language
morphPhonologyV lang = join $ choice [ vowelShift lang
                                     --, insertVowel lang
                                     --, deleteVowel lang
                                     ]

-- this needs to do something.
{-
insertVowel :: Language -> RVar Language
insertVowel land = do
-}


-- shifts all vowels around a "loop"
-- Doesn't take into account vowel length or tone
-- makes of loop of either all rounded or unrounded vowels, never any "crossover"
-- limited to manner and place
vowelShift :: Language -> RVar Language
vowelShift lang = do
  let (heights, backs, _, _, _) = getVMap lang
  let vowels = nub $ map (\(Vowel h b _ _ _) -> (h,b)) (getVInv lang)
  startPoint <- choice vowels
  loop <- makeVowelLoop startPoint vowels []
  let langN = shiftVowels loop lang
  return $ trace (show loop) langN

shiftVowels :: [(Height, Backness)] -> Language -> Language
shiftVowels loop lang = lang{getRoots = rootsN, getManSyss = manSyssN} where
  roots = getRoots lang
  manSyss = getManSyss lang
  rootsN = map (\(x, Morpheme y) -> (,) x (Morpheme (map (shiftVowel loop) y))) roots
  manSyssN = map (\(ManifestSystem x y z) -> ManifestSystem x y (map (\(Morpheme w, v) -> (,) (Morpheme (map (shiftVowel loop) w)) v) z)) manSyss

shiftVowel :: [(Height, Backness)] -> Phoneme -> Phoneme
shiftVowel _ x@Consonant{} = x
shiftVowel _ x@Diphthong{} = x
shiftVowel loop vowel = case dropWhile (/= (vheight vowel, vbackness vowel)) loop of
  (_:next:_)  -> vowel{vheight = fst next, vbackness = snd next}
  [_]         -> vowel{vheight = fst (head loop), vbackness = snd (head loop)}
  []          -> vowel

makeVowelLoop :: (Height, Backness) -> [(Height, Backness)] -> [(Height, Backness)] -> RVar [(Height, Backness)]
makeVowelLoop start vowels [] = do
  let noStartVowels = vowels \\ [start] --saves us from having no loop
  let next = minimumBy (comparing (vowelDistance start)) noStartVowels
  makeVowelLoop start vowels [next]
makeVowelLoop start vowels loop
  | start == head loop = return loop
  | otherwise = do
  let uniqueVowels = vowels \\ loop
  let next = minimumBy (comparing (vowelDistance (head loop))) uniqueVowels --it's this part that prevents unround-round crossover in the loop
  makeVowelLoop start vowels (next:loop)


-- manhattan distance on height and backness
vowelDistance :: (Height, Backness) -> (Height, Backness) -> Int
vowelDistance (h1,b1) (h2,b2) = abs (fromEnum h1 - fromEnum h2)
                                    + abs (fromEnum b1 - fromEnum b2)
                                  --  + 3 * abs (fromEnum r1 - fromEnum r2)

{- holding off on this, exploring sound changes...
-- merging/splitting vowel heights
morphHeight :: Language -> RVar Language
morphHeight lang = do
  let (heights,_,_,_,_) = getVMap lang
  let opts = [ (splitHeight MID [CLOSE, OPEN] lang, [MID] == heights)
             , (splitHeight MID [CLOSE, MID, OPEN] lang, [MID] == heights)
             , (splitHeight MID [CLOSEMID, OPENMID] lang, [MID] == heights)
             , (splitHeight MID [CLOSEMID, MID, OPENMID] lang, [MID] == heights)

             , (mergeHeight [NASAL] MID lang, NASAL `notElem` manners)

-- merging/splitting vowel backnesses
morphBackness :: Language -> RVar Language
morphBackness lang = do
  let (_,backs,_,_,_) = getVMap lang
  let opts = [ (insertManner NASAL lang, NASAL `notElem` manners)
             ,
             ]
-}
