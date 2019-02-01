module HelperFunctions
( choice_
, isVowel
, isConsonant
, shuffleLists
, replicateUntilM
, safeSample
, safeSample2
, safeChoice2
, safeChoices2
, choices2
, randomSubset
, safeSampleSet
, triangle
, triangleChoice
, zipf
, zipfChoice
) where

import ClassyPrelude

import Data.RVar
import Data.Random hiding (sample, gamma)
import Data.Random.Extras
import Data.Random.Distribution.Triangular
import Data.Random.Distribution.Categorical

import Data.Random.Lift as R (lift)

import Data.Phoneme

isVowel :: Phoneme -> Bool
isVowel Vowel{} = True
isVowel _ = False

isConsonant :: Phoneme -> Bool
isConsonant Consonant{} = True
isConsonant _ = False

shuffleLists :: [a] -> [a] -> [a]
shuffleLists [] ys = ys
shuffleLists (x:xs) ys = x : shuffleLists ys xs

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

triangle :: Int -> Int -> RVar Int
triangle n x
  | n == x = return n
  | otherwise = do
    y <- rvar (Triangular (fromIntegral n :: Double) (fromIntegral n :: Double) (fromIntegral x :: Double))
    return $ round y

-- Should use this for phoneme selection
triangleChoice :: [a] -> RVar (Maybe a)
triangleChoice xs = do
  n <- triangle 0 (length xs)
  return $ index xs n

-- Zipf Distribution stuff
{-zipfPMF :: Float -> Int -> Int -> Float
zipfPMF s n k = (1 / (fromIntegral k ** s)) / harm n s

harm :: Int -> Float -> Float
harm n m = sum $ map (\k -> 1 / (fromIntegral k ** m)) [1..n]-}

zipfPMF :: Float -> Int -> Int -> Float
zipfPMF 1 n r = zipfPMF 1.001 n r -- 1.0 isn't defined
zipfPMF a n r = (r_ ** (-a)) / (((n_ ** (-a)) * (0.5 + (n_/(1 - a)))) + ((2 ** (-a)) * (0.5 - (2/(1 - a)))) + 1) where
  n_ = fromIntegral n
  r_ = fromIntegral r

zipf :: Float -> Int -> RVar Int
zipf s n = categorical $ map (\k -> (zipfPMF s n k, k)) [1..n]

zipfChoice :: Float -> [a] -> RVar (Maybe a)
zipfChoice s xs
  | s <= 0 = return Nothing
  | otherwise = do
  n <- zipf s (length xs)
  return $ index xs (n-1)

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
