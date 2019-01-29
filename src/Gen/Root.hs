module Gen.Root
( makeRootDictionary
, makeRoot
, makeSemiticRoot
, makeTransfix
) where

import ClassyPrelude hiding (Word)

import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Control.Monad as R (replicateM)

import Data.Phoneme
import Data.Word
import Data.Inflection
import Data.Other

import LoadStuff
import HelperFunctions

-- Generate root dictionary - atomic meanings
makeRootDictionary :: MeaningData -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> (Int, Int) -> [(LexCat, Int, Int, Int, Int)] -> RVar [Morpheme]
makeRootDictionary mData onsets nucs codas tones set numPerLC = do
  foo <- mapM (\(lc,_,_,_,sr) -> makeRootDictionary_ mData lc sr onsets nucs codas tones set) numPerLC
  return $ concat foo

makeRootDictionary_ :: MeaningData -> LexCat -> Int -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> (Int, Int) -> RVar [Morpheme]
makeRootDictionary_ mData Noun sr onsets nucs codas tones set = makeRootDictionary__ Noun (inputNouns mData) sr onsets nucs codas tones set
makeRootDictionary_ mData Verb sr onsets nucs codas tones set = makeRootDictionary__ Verb (inputVerbs mData) sr onsets nucs codas tones set
makeRootDictionary_ mData Adj sr onsets nucs codas tones set = makeRootDictionary__ Adj (inputAdjs mData) sr onsets nucs codas tones set
makeRootDictionary_ mData Adpo sr onsets nucs codas tones set = makeRootDictionary__ Adpo (inputAdpos mData) sr onsets nucs codas tones set
makeRootDictionary_ _ _ _ _ _ _ _ _ = return []

makeRootDictionary__ :: LexCat -> [Text] -> Int -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> (Int, Int) -> RVar [Morpheme]
makeRootDictionary__ lc foo 0 onsets nucs codas tones set = mapM (\i -> MorphemeS (Meaning lc i) Root <$> makeRoot onsets nucs codas tones set) foo
makeRootDictionary__ lc foo _ onsets _ _ _ (_, nr) = mapM (\i -> SemiticRoot (Meaning lc i) Root <$> makeSemiticRoot onsets nr) foo

-- Generate a root morphemes given vowels, consonant clusters, and some settings
makeRoot :: [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> (Int, Int) -> RVar [Syllable]
makeRoot onsets nucs codas tones (ns,xs) = do
  -- decide how many syllables in the morpheme
  s <- uniform ns xs
  syllables <- R.replicateM s (makeRootSyllable onsets nucs codas tones)

  -- assign stress
  assignStress syllables

makeRootSyllable :: [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> RVar Syllable
makeRootSyllable onsets nucs codas tones = do
  onset <- choice onsets
  nuclei <- choice nucs
  coda <- choice codas
  tone <- choice tones
  return $ Syllable onset nuclei coda tone NONES

assignStress :: [Syllable] -> RVar [Syllable]
assignStress [] = return []
assignStress [syll] = return [syll] -- no stress on single syllable words
assignStress sylls@[_, _] = do
    i <- uniform 0 1
    return $ foobar i sylls (\x -> x{getStress = PRIMARYS})
assignStress sylls = do
  let inds = [0..(length sylls - 1)]
  (inds_, prim) <- fromMaybe (return ([], 0)) (choiceExtract inds)
  (_, secon) <- fromMaybe (return ([], 0)) (choiceExtract inds_)
  let sylls_ = foobar prim sylls (\x -> x{getStress = PRIMARYS})
  let syllsN = foobar secon sylls_ (\x -> x{getStress = SECONDARYS})
  return syllsN

-- Applies a function to the n-th element of a list, and then returns the list
-- It's hard to believe this doesn't exist already
foobar :: Int -> [a] -> (a -> a) -> [a]
foobar n xs f
  | n >= length xs = xs
  | otherwise =  a ++ [f b] ++ bs where
  (a,b:bs) = splitAt n xs


makeSemiticRoot :: [ConsCluster] -> Int -> RVar [[Phoneme]]
makeSemiticRoot conclusts nr = do
  foo <- R.replicateM nr (choice conclusts)
  return foo

makeTransfix :: [ConsCluster] -> [Phoneme] -> [ConsCluster] -> Int -> RVar [[Phoneme]]
makeTransfix onsets nucs codas nr = do
  let t = nr + 1
  R.replicateM t (makeTransfix_ onsets nucs codas)

makeTransfix_ :: [ConsCluster] -> [Phoneme] -> [ConsCluster] -> RVar [Phoneme]
makeTransfix_ onsets nucs codas = do
  onset <- choice onsets
  nuclei <- choice nucs
  coda <- choice codas
  return $ [nuclei]


-- Gen.Meaning?
-- special generators
makeColorSystem :: RVar [Text]
makeColorSystem = do
  ncolors <- uniform 0 11 :: RVar Int
  case ncolors of
    0 -> return []
    1 -> sample 1 ["white", "black"]
    2 -> return ["white", "black"]
    3 -> return ["white", "black", "red"]
    4 -> (++) ["white", "black", "red"] <$> sample 1 ["yellow", "green"]
    5 -> return ["white", "black", "red", "yellow", "green"]
    6 -> return ["white", "black", "red", "yellow", "green", "blue"]
    7 -> return ["white", "black", "red", "yellow", "green", "blue", "brown"]
    8 -> (++) ["white", "black", "red", "yellow", "green", "blue", "brown"] <$> sample 1 ["purple", "pink", "orange", "gray"]
    9 -> (++) ["white", "black", "red", "yellow", "green", "blue", "brown"] <$> sample 2 ["purple", "pink", "orange", "gray"]
    10 -> (++) ["white", "black", "red", "yellow", "green", "blue", "brown"] <$> sample 3 ["purple", "pink", "orange", "gray"]
    11 -> return ["white", "black", "red", "yellow", "green", "blue", "brown", "pueple", "pink", "orange", "gray"]
    _ -> return []

makeNumberSystem :: RVar [Text]
makeNumberSystem = do
  base <- choice [2, 4, 5, 6, 8, 10, 12, 15, 16, 20, 40, 60]
  return $ map tshow ([0..base] ++ ((^) <$> [base] <*> [2..6]))
