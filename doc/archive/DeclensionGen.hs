module DeclensionGen
( makeDeclension
, makeSeedTemplate
, makeTriDeclension
, numTemplate
, subsequencesOfSize
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
  case2 = join $ choice [ ]

  -- Exactly 3 grammatical features
  case3 = join $ choice [
                        ]

  -- 4 or more grammatical features
  case4 = join $ choice [
                        ]




-- Slow down explaination
-- Given the grammar system, find all possible templates <- up to 2187
-- Given the template, find all possible "seeds" which are just a syllable-generating set of phonemes <- ridiculously large number
-- The number of seeds can be so large because I use subsequencesOfSize three times (one for each C V C) and then make the combinations
-- Solution: Implement recursion and randomess; make a random template, make a random seed, repeat if it isn't valid, come back with a false after 1000 iterations or something

makeSeedTemplate :: GrammarSystem -> [MaybeConsonant] -> [Vowel] -> [Syllable] -> RVar (([MaybeConsonant],[Vowel],[MaybeConsonant]), Template)
makeSeedTemplate gramSys consList vowsList syllList = output where
  -- number of consonants
  numCons = length consList
  -- number of vowels
  numVow = length vowsList
  -- number of syllables
  numSyll = length syllList

  -- make all possible templates (actually below only does many to one mapping so no Number -> C1+V)
  tempList = makeAllTemplates gramSys
  -- filter out all templates that result in needing more consonants/vowels/syllables than actual number of consonants/vowels/syllable
  tempListFilt = filter (\x -> fst3 (numTemplate gramSys x) <= numCons &&
                                     mid3 (numTemplate gramSys x) <= numVow &&
                                     lst3 (numTemplate gramSys x) <= numCons &&
                                     fst3 (numTemplate gramSys x) * mid3 (numTemplate gramSys x) * lst3 (numTemplate gramSys x) <= numSyll) tempList
  -- retrieve the number of phonemes per syllable place
  tempNum = map (numTemplate gramSys) tempListFilt
  -- find the possible seeds given the number of phonemes per syllable place
  seedList = map (getPossibleSeeds consList vowsList syllList) tempNum
  -- package the seeds and the template
  tempSeedList = zip seedList tempListFilt
  -- filter out blanks
  tempSeedListFilt = filter (\(x,_) -> not (null x)) tempSeedList
  output = do
    -- choose a seeds-template config
    (seeds, template) <- choice tempSeedListFilt
    -- choose a seed
    seed <- choice seeds
    return (seed, template)

-- should return valid phoneme "seeds" which will be mapped to grammatical categories...
getPossibleSeeds :: [MaybeConsonant] -> [Vowel] -> [Syllable] -> (Int, Int, Int) -> [([MaybeConsonant],[Vowel],[MaybeConsonant])]
getPossibleSeeds cons vows syllList (c1Int, vInt, c2Int) = output where
  --list of all i length combos of consonants
  subc1 = subsequencesOfSize c1Int cons
  --list of all j length combos of vowels
  subv = subsequencesOfSize vInt vows
  --list of all k length combos of consonants
  subc2 = subsequencesOfSize c2Int cons
  --all combinations of the above (list of lists) [([],[],[])]
  allSeeds = (,,) <$> subc1 <*> subv <*> subc2
  --filter out all seeds that generate impossible syllables
  filtSeeds = filter (\x -> all (`elem` syllList) (Syllable <$> fst3 x <*> mid3 x <*> lst3 x)) allSeeds
  output = filtSeeds

-- Declension with one/many to one mapping. Can be anything as long as one grammatical category is mapped to a single syllable place
makeTriDeclension :: GrammarSystem -> ([MaybeConsonant],[Vowel],[MaybeConsonant]) -> Template -> Declension
makeTriDeclension gramSys (con1s, vows, con2s) temp = output where

  (g, a, c, n, h, d, s) = cleanGrammarSys gramSys
  (gNum, aNum, cNum, nNum, hNum, dNum, sNum) = (length g, length a, length c, length n, length h, length d, length s)
  (Template (gInt, aInt, cInt, nInt, hInt, dInt, sInt)) = temp

  -- this gets the combos
  combos = makeCombos gramSys
  -- c1's first
  -- make a new list of combos using only the c1 grammatical categories
  c1NewCombos = (,,,,,,) <$> c1g <*> c1a <*> c1c <*> c1n <*> c1h <*> c1d <*> c1s
  c1g | gInt /= 1 = [Nothing] | otherwise = g
  c1a | aInt /= 1 = [Nothing] | otherwise = a
  c1c | cInt /= 1 = [Nothing] | otherwise = c
  c1n | nInt /= 1 = [Nothing] | otherwise = n
  c1h | hInt /= 1 = [Nothing] | otherwise = h
  c1d | dInt /= 1 = [Nothing] | otherwise = d
  c1s | sInt /= 1 = [Nothing] | otherwise = s

  -- assign each new combo a con1s
  testcombos = zip con1s c1NewCombos

  -- then match the new combos to the orginal combo list somehow
  declc1 = fooBar combos testcombos []

  fooBar :: [(Maybe Gender, Maybe Animacy, Maybe Case, Maybe Number, Maybe Honorific, Maybe Definiteness, Maybe Specificity)]
         -> [(MaybeConsonant, (Maybe Gender, Maybe Animacy, Maybe Case, Maybe Number, Maybe Honorific, Maybe Definiteness, Maybe Specificity))]
         -> [(MaybeConsonant, (Maybe Gender, Maybe Animacy, Maybe Case, Maybe Number, Maybe Honorific, Maybe Definiteness, Maybe Specificity))]
         -> [(MaybeConsonant, (Maybe Gender, Maybe Animacy, Maybe Case, Maybe Number, Maybe Honorific, Maybe Definiteness, Maybe Specificity))]
  fooBar [] _ declc1 = declc1
  fooBar combos testcombos declc1 = fooBar (tail combos) testcombos (output:declc1) where
    match = filter (\(_,x) -> theFilter (head combos) x) testcombos
    output = (fst $ head match, head combos)

  -- v's second
  -- make a new list of combos using only the v grammatical categories
  vNewCombos = (,,,,,,) <$> vg <*> va <*> vc <*> vn <*> vh <*> vd <*> vs
  vg | gInt /= 2 = [Nothing] | otherwise = g
  va | aInt /= 2 = [Nothing] | otherwise = a
  vc | cInt /= 2 = [Nothing] | otherwise = c
  vn | nInt /= 2 = [Nothing] | otherwise = n
  vh | hInt /= 2 = [Nothing] | otherwise = h
  vd | dInt /= 2 = [Nothing] | otherwise = d
  vs | sInt /= 2 = [Nothing] | otherwise = s

  -- assign each new combo a con1s
  testcombos2 = zip vows vNewCombos

  -- then match the new combos to the orginal combo list somehow
  declv = fooBar2 declc1 testcombos2 []

  fooBar2 :: [(MaybeConsonant, (Maybe Gender, Maybe Animacy, Maybe Case, Maybe Number, Maybe Honorific, Maybe Definiteness, Maybe Specificity))]
         -> [(Vowel, (Maybe Gender, Maybe Animacy, Maybe Case, Maybe Number, Maybe Honorific, Maybe Definiteness, Maybe Specificity))]
         -> [((MaybeConsonant, Vowel), (Maybe Gender, Maybe Animacy, Maybe Case, Maybe Number, Maybe Honorific, Maybe Definiteness, Maybe Specificity))]
         -> [((MaybeConsonant, Vowel), (Maybe Gender, Maybe Animacy, Maybe Case, Maybe Number, Maybe Honorific, Maybe Definiteness, Maybe Specificity))]
  fooBar2 [] _ declv = declv
  fooBar2 declc1 testcombos declv = fooBar2 (tail declc1) testcombos (output:declv) where
    match = filter (\(_,x) -> theFilter (snd $ head declc1) x) testcombos
    output = ((fst $ head declc1, fst $ head match), snd $ head declc1)

  -- c2's third
  -- make a new list of combos using only the c2 grammatical categories
  c2NewCombos = (,,,,,,) <$> c2g <*> c2a <*> c2c <*> c2n <*> c2h <*> c2d <*> c2s
  c2g | gInt /= 3 = [Nothing] | otherwise = g
  c2a | aInt /= 3 = [Nothing] | otherwise = a
  c2c | cInt /= 3 = [Nothing] | otherwise = c
  c2n | nInt /= 3 = [Nothing] | otherwise = n
  c2h | hInt /= 3 = [Nothing] | otherwise = h
  c2d | dInt /= 3 = [Nothing] | otherwise = d
  c2s | sInt /= 3 = [Nothing] | otherwise = s

  -- assign each new combo a con1s
  testcombos3 = zip con2s c2NewCombos

  -- then match the new combos to the orginal combo list somehow
  output = Declension (fooBar3 declv testcombos3 [])

  fooBar3 :: [((MaybeConsonant, Vowel), (Maybe Gender, Maybe Animacy, Maybe Case, Maybe Number, Maybe Honorific, Maybe Definiteness, Maybe Specificity))]
         -> [(MaybeConsonant, (Maybe Gender, Maybe Animacy, Maybe Case, Maybe Number, Maybe Honorific, Maybe Definiteness, Maybe Specificity))]
         -> [(Syllable, (Maybe Gender, Maybe Animacy, Maybe Case, Maybe Number, Maybe Honorific, Maybe Definiteness, Maybe Specificity))]
         -> [(Syllable, (Maybe Gender, Maybe Animacy, Maybe Case, Maybe Number, Maybe Honorific, Maybe Definiteness, Maybe Specificity))]
  fooBar3 [] _ declc2 = declc2
  fooBar3 declv testcombos declc2 = fooBar3 (tail declv) testcombos (output:declc2) where
    match = filter (\(_,x) -> theFilter (snd $ head declv) x) testcombos
    output = (uncurry Syllable (fst $ head declv) (fst $ head match), snd $ head declv)


-- filter used for trideclensions
theFilter maincombo testcombo = result where
  (g1, a1, c1, n1, h1, d1, s1) = maincombo
  (g2, a2, c2, n2, h2, d2, s2) = testcombo
  result= and [g1 == g2 || isNothing g2 , a1 == a2 || isNothing a2, c1 == c2 || isNothing c2, n1 == n2 || isNothing n2, h1 == h2 || isNothing h2, d1 == d2 || isNothing d2, s1 == s2 || isNothing s2]


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
  c1 = map (\x -> if x /= 1 then 0; else 1) [gInt, aInt, cInt, nInt, hInt, dInt, sInt]
  c1Int = product $ filter (/=0) $ zipWith (*) c1 ints
  v = map (\x -> if x /= 2 then 0; else 1) [gInt, aInt, cInt, nInt, hInt, dInt, sInt]
  vInt = product $ filter (/=0) $ zipWith (*) v ints
  c2 = map (\x -> if x /= 3 then 0; else 1) [gInt, aInt, cInt, nInt, hInt, dInt, sInt]
  c2Int = product $ filter (/=0) $ zipWith (*) c2 ints


-- Other utility functions below (should probably be moved to another .hs)
fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

mid3 :: (a,b,c) -> b
mid3 (a,b,c) = b

lst3 :: (a,b,c) -> c
lst3 (a,b,c) = c

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs = let l = length xs
                          in if n>l then [] else subsequencesBySize xs !! (l-n)
 where
   subsequencesBySize [] = [[[]]]
   subsequencesBySize (x:xs) = let next = subsequencesBySize xs
                             in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])
