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

  -- number of consonants
  numCons = length consList

  -- number of vowels
  numVow = length vowsList

  -- number of syllables
  numSyll = length syllList

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
  case2 = join $ choice $ [ makeRndDeclension gramSys syllList ] ++ vc2S ++ c1vS ++ c1c2S ++ c1S ++ vS ++ c2S
  -- Need to find all the pairs of C1-VC2 where length C1 >= first category and length VC2 >= second category, or the reverse
  -- Need to find all the pairs of X-Y where length X >

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
  -- make all possible templates (actually below only does many to one mapping so no Number -> C1+V)
  tempList = makeAllTemplates gramSys
  tempNum = map (numTemplate gramSys) tempList
  tempListNum = zip tempNum tempList
  -- filter out all templates that result in needed consonants being larger than actual number of consonants
  -- filter out template that result in needed vowels being larger than actual number of vowels
  -- filter out templates that result in more syllables than what we have
  tempListNumFilt = filter (\((x,y,z),_) -> x <= numCons && y <= numVow && z <= numCons && (x*y*z) <= numSyll) tempListNum


  subsequencesOfSize :: Int -> [a] -> [[a]]
  subsequencesOfSize n xs = let l = length xs
                            in if n>l then [] else subsequencesBySize xs !! (l-n)
   where
     subsequencesBySize [] = [[[]]]
     subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                               in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])

  fooBar :: (Int, Int, Int) -> [MaybeConsonant] -> [Vowel] -> [Syllable] -> [[Syllable]]
  fooBar (c1Int, vInt, c2Int) cons vows syllList = output where
    subc1 = subsequencesOfSize c1Int cons
    subv = subsequencesOfSize vInt vows
    subc2 = subsequencesOfSize c2Int cons
    co = (,,) <$> subc1 <*> subv <*> subc2
    listListSyll = (zipWith3 Syllable (map fst3 co) (map mid3 co) (map lst3 co))
    output = filter (\x -> and (flip . map (elem syllList)) x) listListSyll

  fst3 :: (a,b,c) -> a
  fst3 (a,b,c) = c

  mid3 :: (a,b,c) -> b
  mid3 (a,b,c) = b

  lst3 :: (a,b,c) -> c
  lst3 (a,b,c) = c

  -- can i pick (1-Up to number of) consonants, (1-Up to number of vowels) vowels, and (1 - Up to number of) consonants that form (1 - up to number of syllables)
  -- find (length c1 needed) consonants, (length v needed) vowels, and (length c2 needed) consonants that all form syllables that are in syllList

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

-- Template stuff below
-- Makes all possible templates
-- like all categories assigned to C1, or 1/7 assigned to C1 and 2/7 assigned to V and 4/7 assigned to C3
makeAllTemplates :: GrammarSystem -> [Template]
makeAllTemplates (GrammarSystem g a c n h d s) = Template <$> allTemplates where

  assign cat
    | isNothing cat = [0]
    | otherwise = [1,2,3]

  allTemplates = (,,,,,,) <$> assign g <*> assign a <*> assign c <*> assign n <*> assign h <*> assign d <*> assign s

-- Given a template, how many seperate phonemes are needed per place (C1, V, C2)
numTemplate :: GrammarSystem -> Template -> (Int, Int, Int)
numTemplate (GrammarSystem g a c n h d s) (Template (gInt, aInt, cInt, nInt, hInt, dInt, sInt)) = (c1Int, vInt, c2Int) where
  ints = [maybe 1 length g, maybe 1 length a, maybe 1 length c, maybe 1 length n, maybe 1 length h, maybe 1 length d, maybe 1 length s]
  c1 = map (\x -> if x /= 1 then 0; else x) [gInt, aInt, cInt, nInt, hInt, dInt, sInt]
  c1Int = product $ zipWith (*) c1 ints
  v = map (\x -> if x /= 2 then 0; else x) [gInt, aInt, cInt, nInt, hInt, dInt, sInt]
  vInt = product $ zipWith (*) v ints
  c2 = map (\x -> if x /= 3 then 0; else x) [gInt, aInt, cInt, nInt, hInt, dInt, sInt]
  c2Int = product $ zipWith (*) c2 ints
