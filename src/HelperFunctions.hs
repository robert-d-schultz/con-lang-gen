module HelperFunctions
( choice_
, isVowel
, isConsonant
, replicateUntilM
, safeSample
, safeSample2
, safeChoice2
, safeChoices2
, choices2
, randomSubset
, safeSampleSet
) where

import ClassyPrelude

import Data.RVar
import Data.Random hiding (sample)
import Data.Random.Extras

import Data.Random.Lift as R (lift)

import Data.Phoneme

isVowel :: Phoneme -> Bool
isVowel Vowel{} = True
isVowel _ = False

isConsonant :: Phoneme -> Bool
isConsonant Consonant{} = True
isConsonant _ = False

-- RVar stuff
-- Weighs the first argument by N
choice_ :: Int -> a -> a -> RVar a
choice_ wei fir sec = choice (sec : replicate wei fir)


-- First argument contains the weights of the second argument
-- The two have to be equal in size
choiceWeight :: [Int] -> [a] -> Maybe (RVar a)
choiceWeight [] _ = Nothing
choiceWeight _ [] = Nothing
choiceWeight ws xs
  | length ws /= length xs = Nothing
  | otherwise = Just $ choice $ concat $ zipWith replicate ws xs

-- sample but without worrying about empty list exception
safeSample :: Int -> [a] -> Maybe (RVar [a])
safeSample n xs
  | length xs >= n = Just $ sample n xs
  | otherwise = Nothing

-- safeSample but RVar on outside
safeSample2 :: Int -> [a] -> RVar (Maybe [a])
safeSample2 n xs
  | length xs >= n = Just <$> sample n xs
  | otherwise = return Nothing

-- safeChoice but RVar on outside
safeChoice2 :: [a] -> RVar (Maybe a)
safeChoice2 [] = return Nothing
safeChoice2 xs = Just <$> choice xs

-- safeChoice but RVar on outside
safeChoices2 :: Int -> [a] -> RVar (Maybe [a])
safeChoices2 _ [] = return Nothing
safeChoices2 n xs = Just <$> choices n xs

-- choices but returns [] on empty list
choices2 :: Int -> [a] -> RVar [a]
choices2 _ [] = return []
choices2 n xs = choices n xs

randomSubset :: [a] -> RVar [a]
randomSubset [] = return []
randomSubset xs = join $ flip sample xs <$> uniform 0 (length xs)

safeSampleSet :: Ord a => Int -> Set a -> Maybe (RVar (Set a))
safeSampleSet n xs = do
    foo <- safeSample n (setToList xs)
    return $ setFromList <$> foo

choiceExtract2 :: [a] -> RVar (Maybe ([a], a))
choiceExtract2 [] = return Nothing
choiceExtract2 xs = extract xs <$> uniform 0 (length xs - 1)

extract :: [a] -> Int -> Maybe ([a], a)
extract s i | null r    = Nothing
            | otherwise = Just (a ++ c, b)
    where (a, r) = splitAt i s
          (b : c) = r


-- Doesn't work?
-- This is like replicateM, except it replicates until it either hits the amount
-- you want or it reaches a specified limit (so it doesn't go forever).
-- This is actually pretty useful, I'm suprised it doesn't exist already.
replicateUntilM :: Monad m => (a -> Bool) -> Int -> Int -> m a -> m [a]
replicateUntilM _ 0 _ _ = return []
replicateUntilM _ _ 0 _ = return []
replicateUntilM test i limit thing = do
  revealThing <- thing
  itFails <- replicateUntilM test i (limit-1) thing
  itSucceeds <- replicateUntilM test (i-1) (limit-1) thing
  if test revealThing then return (revealThing:itSucceeds) else return itFails
