module HelperFunctions
( choice_
, isVowel
, isConsonant
) where

import Data.RVar
import Data.Random.Extras

import Data.Phoneme

-- weighs the first argument by N
choice_ :: Int -> a -> a -> RVar a
choice_ wei fir sec = choice (sec : replicate wei fir)

isVowel :: Phoneme -> Bool
isVowel Vowel{} = True
isVowel _ = False

isConsonant :: Phoneme -> Bool
isConsonant Consonant{} = True
isConsonant _ = False
