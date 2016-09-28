module DeclensionGen
( makeDeclension
) where

import Prelude hiding (Word)
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Control.Monad
import Data.Maybe

import PhonemeInventoryGen
import PhonemeType
import PhonotacticsGen
import OtherData
import GrammarType
import GrammarGen
import WordGen

-- Generates declension
makeDeclension :: GrammarSystem -> [Syllable] -> [MaybeConsonant] -> [Vowel] -> RVar Declension
makeDeclension gramSys syllList consList vowsList = output where
  GrammarSystem g a c n h d s = gramSys

  -- number of expressed features
  num = length (filter id [isJust g, isJust a, isJust c, isJust n, isJust h, isJust d, isJust s])

  -- number of individual expressed features
  numInd = maybe 0 length g + maybe 0 length a + maybe 0 length c + maybe 0 length n + maybe 0 length h + maybe 0 length d + maybe 0 length s

  -- number of combinations of features
  numCombo = length $ makeCombos gramSys

  output = case num of 0 -> case0
                       1 -> case1
                       2 -> case2
                       3 -> case3
                       _ -> case4

  -- Zero grammatical features, it should probably never get here
  case0 = Declension . (:[]) <$> ((,) <$> choice syllList <*> return (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing))

  -- One grammatical feature
  case1 = join $ choice $ [ makeRndDeclension gramSys syllList ] ++ vc2S ++ c1vS ++ c1c2S ++ c1S ++ vS ++ c2S

  vc2 = filter (\x -> length x >= numCombo) $ findVC2Syllables <$> vowsList <*> consList <*> [syllList]
  vc2S
    | not (null vc2) = [makeCase1Declension gramSys vc2 ]
    | otherwise = []

  c1v = filter (\x -> length x >= numCombo) $ findC1VSyllables <$> consList <*> vowsList <*> [syllList]
  c1vS
    | not (null c1v) = [makeCase1Declension gramSys c1v ]
    | otherwise = []

  c1c2 = filter (\x -> length x >= numCombo) $ findC1C2Syllables <$> consList <*> consList <*> [syllList]
  c1c2S
    | not (null c1c2) = [makeCase1Declension gramSys c1c2 ]
    | otherwise = []

  c1 = filter (\x -> length x >= numCombo) $ findC1Syllables <$> consList <*> [syllList]
  c1S
    | not (null c1) = [makeCase1Declension gramSys c1 ]
    | otherwise = []

  v = filter (\x -> length x >= numCombo) $ findVSyllables <$> vowsList <*> [syllList]
  vS
    | not (null v) = [makeCase1Declension gramSys v ]
    | otherwise = []

  c2 = filter (\x -> length x >= numCombo) $ findC1Syllables <$> consList <*> [syllList]
  c2S
    | not (null c2) = [makeCase1Declension gramSys c2 ]
    | otherwise = []

  -- 2 grammatical features
  case2 = join $ choice [ makeRndDeclension gramSys syllList ] ++ vc2S ++ c1vS ++ c1c2S ++ c1S ++ vS ++ c2S

                  --vary C1 for one, V for the other, hold C2 constant
                  --vary C2 for one, C1 for the other, hold V constant
                  --vary V for one, C2 for the other, hold C1 constant
                  --etc.

  -- Exactly 3 grammatical features
  case3 = join $ choice [ makeRndDeclension gramSys syllList
                        ]

  -- 4 or more grammatical features
  case4 = join $ choice [ makeRndDeclension gramSys syllList
                        ]

-- Random syllable for each combo
makeRndDeclension :: GrammarSystem -> [Syllable] -> RVar Declension
makeRndDeclension gramSys syllList = Declension <$> output where
  -- this gets the combos
  combos = makeCombos gramSys
  -- this gets the syllables
  selected = sample (length combos) syllList
  -- this zips the combos and syllables
  output = zip <$> selected <*> return combos

-- Declension for when one grammatical category is expressed
-- Hold something (C1, C1V, etc.) constant, vary the other thing
makeCase1Declension :: GrammarSystem -> [[Syllable]] -> RVar Declension
makeCase1Declension gramSys possible = Declension <$> output where
  -- get the combos
  combos = makeCombos gramSys
  -- pick the final C1/V/C2/C1V/VC2/C1C2
  final = choice possible
  -- select the final syllables
  selected = join $ sample (length combos) <$> final
  -- zip the combos and syllables
  output = zip <$> selected <*> return combos

-- have to filter by one thing and then filter again - i think
makeCase2Declension :: GrammarSystem -> [[Syllable]] -> RVar Declension
makeCase2Declension gramSys possible = Declension <$> output where
  -- get the combos
  combos = makeCombos gramSys
  -- pick the final C1/V/C2/C1V/VC2/C1C2
  final = choice possible
  -- select the final syllables
  selected = join $ sample (length combos) <$> final
  -- zip the combos and syllables
  output = zip <$> selected <*> return combos


-- Combo stuff below
-- Cleans the grammar sys
cleanGrammarSys :: GrammarSystem -> ([Maybe Gender], [Maybe Animacy], [Maybe Case], [Maybe Number], [Maybe Honorific], [Maybe Definiteness], [Maybe Specificity])
cleanGrammarSys gramSys = (g, a, c, n, h, d, s) where
  g = sequenceR (gSys gramSys) []
  a = sequenceR (aSys gramSys) []
  c = sequenceR (cSys gramSys) []
  n = sequenceR (nSys gramSys) []
  h = sequenceR (hSys gramSys) []
  d = sequenceR (dSys gramSys) []
  s = sequenceR (sSys gramSys) []

makeCombos :: GrammarSystem -> [(Maybe Gender, Maybe Animacy, Maybe Case, Maybe Number, Maybe Honorific, Maybe Definiteness, Maybe Specificity)]
makeCombos gramSys = (,,,,,,) <$> g <*> a <*> c <*> n <*> h <*> d <*> s where
  (g, a, c, n, h, d, s) = cleanGrammarSys gramSys

-- Used to make the combos
sequenceR :: Maybe [a] -> [Maybe a] -> [Maybe a]
sequenceR Nothing _ = [Nothing]
sequenceR (Just []) output = output
sequenceR input output = sequenceR (tail <$> input) ((head <$> input):output)


-- Filter stuff below
-- filter all possible syllables based on C1, V, C2 (aka check if a particular syllable is valid)
findC1VC2Syllables syllC1 syllV syllC2 = filter (\x -> getC1 x == syllC1 && getV x == syllV && getC2 x == syllC2)

-- filter all possible syllables based on C1, V
findC1VSyllables syllC1 syllV = filter (\x -> getC1 x == syllC1 && getV x == syllV)

-- filter all possible syllables based on V, C2
findVC2Syllables syllV syllC2 = filter (\x -> getV x == syllV && getC2 x == syllC2)

-- filter all possible syllables based on C1 C2
findC1C2Syllables syllC1 syllC2 = filter (\x -> getC1 x == syllC1 && getC2 x == syllC2)

-- filter all possible syllables based on C1
findC1Syllables syllC1 = filter (\x -> getC1 x == syllC1)

-- filter all possible syllables based on C2
findC2Syllables syllC2 = filter (\x -> getC2 x == syllC2)

-- filter all possible syllables based on V
findVSyllables syllV = filter (\x -> getV x == syllV)
