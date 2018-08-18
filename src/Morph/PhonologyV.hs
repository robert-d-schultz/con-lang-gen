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

import  Debug.Trace

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
-- need to look up how the Great Vowel Shift worked historically, maybe just limit to manner/place
vowelShift :: Language -> RVar Language
vowelShift lang = do
  let (heights, backs, rounds, lengths, tones, excepts) = getVMap lang
  let vowels = (,,) <$> heights <*> backs <*> rounds
  startPoint <- choice vowels
  loop <- makeVowelLoop startPoint vowels []
  let langN = shiftVowels loop lang
  return $ trace (show loop) langN

shiftVowels :: [(Height, Backness, Roundedness)] -> Language -> Language
shiftVowels loop lang = lang{getRoots = rootsN, getManSyss = manSyssN} where
  roots = getRoots lang
  manSyss = getManSyss lang
  rootsN = map (\(x, Morpheme y) -> (,) x (Morpheme (map (shiftVowel loop) y))) roots
  manSyssN = map (\(ManifestSystem x y z) -> ManifestSystem x y (map (\(Morpheme w, v) -> (,) (Morpheme (map (shiftVowel loop) w)) v) z)) manSyss

shiftVowel :: [(Height, Backness, Roundedness)] -> Phoneme -> Phoneme
shiftVowel _ x@Consonant{} = x
shiftVowel _ x@Diphthong{} = x
shiftVowel loop vowel = case dropWhile (/= (vheight vowel, vbackness vowel, vroundedness vowel)) loop of
  (_:next:_)  -> vowel{vheight = (\(x,_,_)->x) next, vbackness = (\(_,x,_)->x) next, vroundedness = (\(_,_,x) -> x) next}
  [_]         -> vowel{vheight = (\(x,_,_)->x) (head loop), vbackness = (\(_,x,_)->x) (head loop), vroundedness = (\(_,_,x) -> x) (head loop)}
  []          -> vowel

makeVowelLoop :: (Height, Backness, Roundedness) -> [(Height, Backness, Roundedness)] -> [(Height, Backness, Roundedness)] -> RVar [(Height, Backness, Roundedness)]
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


--manhattan distance on height and backness... + some stuff for roundedness
vowelDistance :: (Height, Backness, Roundedness) -> (Height, Backness, Roundedness) -> Int
vowelDistance (h1,b1,r1) (h2,b2,r2) = abs (fromEnum h1 - fromEnum h2)
                                    + abs (fromEnum b1 - fromEnum b2)
                                    + 3 * abs (fromEnum r1 - fromEnum r2)


-- calculates an approximate similarity between two phonemes
phonemeDistance :: Phoneme -> Phoneme -> Int
phonemeDistance (Vowel h1 b1 r1 l1 t1) (Vowel h2 b2 r2 l2 t2) = abs (fromEnum h1 - fromEnum h2)
                                                              + abs (fromEnum b1 - fromEnum b2)
                                                              + 3 * abs (fromEnum r1 - fromEnum r2)
phonemeDistance Consonant{} Consonant{} = 0 -- temp
phonemeDistance Diphthong{} Diphthong{} = 0 --this'll be cosine distance or something
phonemeDistance _ _ = 999 --maybe something something semivowels...
