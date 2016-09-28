module WordGen
( makeDictionary
, makeWord
, makeAllSyllables
) where

import Prelude hiding (Word)
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Control.Monad

import PhonemeInventoryGen
import PhonemeType
import PhonotacticsGen
import OtherData

-- Generates words

-- Given number of syllables, consonant phonotactics, and vowel phonotactics, generate a list of words
makeDictionary :: Int -> [Syllable] -> RVar [Word]
makeDictionary n syllList = replicateM n (makeWord syllList)

-- Given number of syllables, consonant phonotactics, and vowel phonotactics, generate a word
makeWord :: [Syllable] -> RVar Word
makeWord syllList = do
  i <- uniform 1 4
  sylls <- replicateM i $ choice syllList
  return $ Word sylls

-- Make all possible syllables
makeAllSyllables :: ([MaybeConsonant], [MaybeConsonant]) -> ([Vowel], [Vowel], [Vowel], [Vowel]) -> [Syllable]
makeAllSyllables (onsetC, codaC) (nucleusV, onsetV, codaV, aloneV) = syllListCVC ++ syllListVC ++ syllListCV ++ syllListV where
  syllListCVC
    | codaC /= [Blank] || onsetC /= [Blank] = Syllable <$> onsetC <*> nucleusV <*> codaC
    | otherwise = []

  syllListVC
    | Blank `elem` codaC = Syllable <$> onsetC <*> codaV <*> [Blank]
    | otherwise = []

  syllListCV
    | Blank `elem` onsetC = Syllable <$> [Blank] <*> onsetV <*> codaC
    | otherwise = []

  syllListV
    | Blank `elem` codaC && Blank `elem` onsetC = Syllable <$> [Blank] <*> aloneV <*> [Blank]
    | otherwise = []

--All below might be redundant
{-
-- Number of vowels for a known C1 and C2
numOfVow :: MaybeConsonant -> MaybeConsonant -> ([Vowel], [Vowel], [Vowel], [Vowel]) -> Int
numOfVow Blank Blank (_,_,_,aloneVT) = length aloneVT
numOfVow Blank Consonant{} (_,onsetVT,_,_) = length onsetVT
numOfVow Consonant{} Blank (_,_,codaVT,_) = length codaVT
numOfVow Consonant{} Consonant{} (nucleusVT,_,_,_) = length nucleusVT

-- Chooses C2 from a known C1 and Vowel
chooseC2 :: MaybeConsonant -> Vowel -> ([MaybeConsonant], [MaybeConsonant]) -> ([Vowel], [Vowel], [Vowel], [Vowel]) -> RVar MaybeConsonant
chooseC2 cons1 vow (onsetC, codaC) (nucleusV, onsetV, codaV, aloneV)
  | (cons1 == Blank) && (vow `elem` aloneV) && (vow `notElem` nucleusV) && (vow `notElem` onsetV) = return Blank
  | (vow `elem` codaV) && (vow `notElem` nucleusV) = return Blank
  | vow `elem` nucleusV = choice codaC

-- Number of C2's for a known C1 and Vowel
numOfC2 :: MaybeConsonant -> Vowel -> ([MaybeConsonant], [MaybeConsonant]) -> ([Vowel], [Vowel], [Vowel], [Vowel]) -> Int
numOfC2 cons1 vow (onsetC, _) (nucleusV, onsetV, codaV, aloneV)
  | (cons1 == Blank) && (vow `elem` aloneV) && (vow `notElem` nucleusV) && (vow `notElem` onsetV) = 0
  | (vow `elem` codaV) && (vow `notElem` nucleusV) = 0
  | vow `elem` nucleusV = length onsetC

-- ChoosesC1 from a known Vowel and C2
chooseC1 :: MaybeConsonant -> Vowel -> ([MaybeConsonant], [MaybeConsonant]) -> ([Vowel], [Vowel], [Vowel], [Vowel]) -> RVar MaybeConsonant
chooseC1 cons2 vow (onsetC, codaC) (nucleusV, onsetV, codaV, aloneV)
  | (cons2 == Blank) && (vow `elem` aloneV) && (vow `notElem` nucleusV) && (vow `notElem` codaV) = return Blank
  | (vow `elem` onsetV) && (vow `notElem` nucleusV) = return Blank
  | vow `elem` nucleusV = choice onsetC

-- Number of C1's for a known Vowel and C2
numOfC1 :: MaybeConsonant -> Vowel -> ([MaybeConsonant], [MaybeConsonant]) -> ([Vowel], [Vowel], [Vowel], [Vowel]) -> Int
numOfC1 cons2 vow (_, codaC) (nucleusV, onsetV, codaV, aloneV)
  | (cons2 == Blank) && (vow `elem` aloneV) && (vow `notElem` nucleusV) && (vow `notElem` codaV) = 0
  | (vow `elem` onsetV) && (vow `notElem` nucleusV) = 0
  | vow `elem` nucleusV = length codaC

-- Choose C1+V's from a known C2
chooseC1V :: MaybeConsonant -> ([MaybeConsonant], [MaybeConsonant]) -> ([Vowel], [Vowel], [Vowel], [Vowel]) -> (RVar MaybeConsonant, RVar Vowel)
chooseC1V cons2 (onsetC, _) vtact = (cons1, vow) where
  cons1 = choice onsetC
  vow = join $ chooseVowel <$> cons1 <*> return cons2 <*> return vtact

-- Number of C1+V's for a known C2
numOfC1V :: MaybeConsonant -> ([MaybeConsonant], [MaybeConsonant]) -> ([Vowel], [Vowel], [Vowel], [Vowel]) -> Int
numOfC1V cons2 (onsetC, _) (nucleusV, onsetV, codaV, aloneV)
  | (cons2 == Blank) && (Blank `notElem` onsetC) = length onsetC * length codaV
  | (cons2 == Blank) && (Blank `elem` onsetC)    = (length onsetC - 1) * length codaV + length aloneV
  | (cons2 /= Blank) && (Blank `notElem` onsetC) = length onsetC * length nucleusV
  | (cons2 /= Blank) && (Blank `elem` onsetC) = (length onsetC - 1) * length nucleusV + length codaV

-- Choose V+C2's from a known C1
chooseVC2 :: MaybeConsonant -> ([MaybeConsonant], [MaybeConsonant]) -> ([Vowel], [Vowel], [Vowel], [Vowel]) -> (RVar MaybeConsonant, RVar Vowel)
chooseVC2 cons1 (_, codaC) vtact = (cons2, vow) where
  cons2 = choice codaC
  vow = join $ chooseVowel <$> return cons1 <*> cons2 <*> return vtact

-- Number of V+C2's for a known C1
numOfVC2 :: MaybeConsonant -> ([MaybeConsonant], [MaybeConsonant]) -> ([Vowel], [Vowel], [Vowel], [Vowel]) -> Int
numOfVC2 cons2 (_, codaC) (nucleusV, onsetV, codaV, aloneV)
  | (cons2 == Blank) && (Blank `notElem` codaC) = length codaC * length onsetV
  | (cons2 == Blank) && (Blank `elem` codaC) = (length codaC - 1) * length onsetV + length aloneV
  | (cons2 /= Blank) && (Blank `notElem` codaC) = length codaC * length nucleusV
  | (cons2 /= Blank) && (Blank `elem` codaC) = (length codaC - 1) * length nucleusV + length codaV

-- Chooses C1+C2's from a known Vowel
chooseC1C2 :: Vowel -> ([MaybeConsonant], [MaybeConsonant]) -> ([Vowel], [Vowel], [Vowel], [Vowel]) -> RVar (MaybeConsonant, MaybeConsonant)
chooseC1C2 vow (onsetC, codaC) (nucleusV, onsetV, codaV, aloneV) = do
  foob <- output
  let (one, two) = foob
  o <- one
  t <- two
  return (o, t) where
    a
      | (vow `elem` aloneV) && (Blank `elem` onsetC) && (Blank `elem` codaC)      = [(return Blank, return Blank)]
      | otherwise             = []
    b
      | (vow `elem` nucleusV) && (Blank `elem` onsetC) && (Blank `elem` codaC)    = [(choice $ filter (/= Blank) onsetC, choice $ filter (/= Blank) codaC)]
      | otherwise             = []
    c
      | (vow `elem` onsetV) && (Blank `elem` onsetC) && (Blank `elem` codaC)      = [(return Blank, choice $ filter (/= Blank) codaC)]
      | otherwise             = []
    d
      | (vow `elem` codaV) && (Blank `elem` onsetC) && (Blank `elem` codaC)       = [(choice $ filter (/= Blank) onsetC, return Blank)]
      | otherwise             = []
    output = choice (a ++ b ++ c ++ d)

-- Number of C1+C2's for a known Vowel
numOfC1C2 :: Vowel -> ([MaybeConsonant], [MaybeConsonant]) -> ([Vowel], [Vowel], [Vowel], [Vowel]) -> Int
numOfC1C2 vow (onsetC, codaC) (nucleusV, onsetV, codaV, aloneV) = output where
  a
    | (vow `elem` aloneV) && (Blank `elem` onsetC) && (Blank `elem` codaC)      = 0
    | otherwise             = 0
  b
    | (vow `elem` nucleusV) && (Blank `elem` onsetC) && (Blank `elem` codaC)    = (length onsetC - 1) * (length codaC - 1)
    | otherwise             = 0
  c
    | (vow `elem` onsetV) && (Blank `elem` onsetC) && (Blank `elem` codaC)      = length codaC - 1
    | otherwise             = 0
  d
    | (vow `elem` codaV) && (Blank `elem` onsetC) && (Blank `elem` codaC)       = length onsetC - 1
    | otherwise             = 0
  e
    | (vow `elem` nucleusV) && (Blank `elem` onsetC) && (Blank `notElem` codaC) = (length onsetC - 1) * length codaC
    | otherwise             = 0
  f
    | (vow `elem` onsetV) && (Blank `elem` onsetC) && (Blank `notElem` codaC)   = length codaC
    | otherwise             = 0
  g
    | (vow `elem` nucleusV) && (Blank `notElem` onsetC) && (Blank `elem` codaC) = length onsetC * (length codaC - 1)
    | otherwise             = 0
  h
    | (vow `elem` codaV) && (Blank `notElem` onsetC) && (Blank `elem` codaC)    = length onsetC
    | otherwise             = 0
  i
    | (vow `elem` nucleusV) && (Blank `notElem` onsetC) && (Blank `notElem` codaC) = length onsetC * length codaC
    | otherwise             = 0

  output = sum [a,b,c,d,e,f,g,h,i]

-- Number of Syllables possible
numOfSyllables :: ([MaybeConsonant], [MaybeConsonant]) -> ([Vowel], [Vowel], [Vowel], [Vowel]) -> Int
numOfSyllables (onsetC, codaC) (nucleusV, onsetV, codaV, aloneV)
  | (Blank `notElem` onsetC) && (Blank `notElem` codaC) = length onsetC * length nucleusV * length codaC
  | (Blank `notElem` onsetC) && (Blank `elem` codaC)    = (length onsetC * length nucleusV * (length codaC - 1)) + (length onsetC * length codaV)
  | (Blank `elem` onsetC) && (Blank `notElem` codaC)    = ((length onsetC - 1) * length nucleusV * length codaC) + (length codaC * length codaV)
  | (Blank `elem` onsetC) && (Blank `elem` codaC)    = ((length onsetC - 1) * length nucleusV * (length codaC - 1)) + ((length codaC -1) * length codaV) + ((length onsetC -1) * length onsetV) + length aloneV
-}
