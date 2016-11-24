module WordGen
( loadMeaningData
, MeaningData(..)
, makeDictionary
, makeMorpheme
) where

import Prelude hiding (Word)
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Control.Monad
import Data.List

import PhonemeGen
import PhonemeData
import PhonotacticsGen
import OtherData
import InflectionData
import GrammarData

-- Input data
data MeaningData = MeaningData
  {
      inputNouns       :: [String]
    , inputVerbs       :: [String]
    , inputAdjs        :: [String]
    , inputAdpos       :: [String]
  }

loadMeaningData :: IO MeaningData
loadMeaningData =  MeaningData
  <$> readMeaning "raw/meanings/nouns.txt"
  <*> readMeaning "raw/meanings/verbs.txt"
  <*> readMeaning "raw/meanings/adjectives.txt"
  <*> readMeaning "raw/meanings/adpositions.txt"

readMeaning :: Read a => FilePath -> IO a
readMeaning = fmap read . readFile

-- Generate words
-- Generate
makeDictionary :: MeaningData -> [Phoneme] -> [[Phoneme]] -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int)) -> RVar [((String, LexCat), Word)]
makeDictionary mData vows sonhier set = concat <$> sequence [n, v, a, p] where
  n = mapM (\i -> (,) <$> ((,) <$> return i <*> return Noun) <*> makeWord vows sonhier set) (inputNouns mData)
  v = mapM (\i -> (,) <$> ((,) <$> return i <*> return Verb) <*> makeWord vows sonhier set) (inputVerbs mData)
  a = mapM (\i -> (,) <$> ((,) <$> return i <*> return Adj) <*> makeWord vows sonhier set) (inputAdjs mData)
  p = mapM (\i -> (,) <$> ((,) <$> return i <*> return Adpo) <*> makeWord vows sonhier set) (inputAdpos mData)

-- Given sonority hierarchy, generate (one) root morpheme for a word
makeWord :: [Phoneme] -> [[Phoneme]] -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int)) -> RVar Word
makeWord vows sonhier ((ns,xs), (ni,xi), (nt,xt), (ne,xe)) = Word . (:[]) <$> makeMorpheme vows sonhier ((ns,xs), (ni,xi), (nt,xt), (ne,xe))

-- Generate a morpheme given vowels, the consonant sonority hierarchy, and some settings
-- So far this only does vowel-nucleus syllables
-- Maybe should have a "completely random" version
makeMorpheme :: [Phoneme] -> [[Phoneme]] -> ((Int, Int), (Int, Int), (Int, Int), (Int, Int)) -> RVar Morpheme
makeMorpheme vows sonhier ((ns,xs), (ni,xi), (nt,xt), (ne,xe)) = do
  -- decide how many syllables in the morpheme
  s <- uniform ns xs
  -- pick the initial consonant cluster
  i <- uniform ni xi
  initial <- makeConsonantCluster i (reverse sonhier) []
  -- pick the ending consonant cluster
  e <- uniform ne xe
  end <- makeConsonantCluster e sonhier []
  -- pick vowels
  vowels <- choices s vows
  -- pick inside consonant clusters
  inter <- replicateM (max (s-1) 0) $ join $ makeConsonantCluster <$> uniform nt xt <*> return (sonhier ++ reverse (init sonhier)) <*> return []
  --shuffle vowels and inside consonants together
  let inside = concat $ transpose [map (:[]) vowels, inter]

  return $ Morpheme $ initial ++ concat inside ++ end

-- Generate a consonant cluster
-- Goes through sonority hierarchy taking consonants from each grouping
makeConsonantCluster :: Int -> [[Phoneme]] -> [Phoneme] -> RVar [Phoneme]
makeConsonantCluster 0 _ out = return out
makeConsonantCluster _ [] out = return out
makeConsonantCluster l sonhier out = do
  i <- uniform 0 1
  newCs <- sample i (head sonhier)
  makeConsonantCluster (l-i) (tail sonhier) (out ++ newCs)
