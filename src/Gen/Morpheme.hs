module Gen.Morpheme
( makeRootMorphemes
, makeMorpheme
, makeConsonantalRoot
, makeInflectionMorphemes
, cleanInflectionSys
, makeTransfix
, pickLemmaMorphemes
, makeDerivationMorphemes
) where

import ClassyPrelude hiding (Word)

import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)

import Data.Phoneme
import Data.Word
import Data.Inflection
import Data.Other

import Gen.ParseTree (generateInflection)
import Out.Sentence (compareInfl)

import LoadStuff
import HelperFunctions

-- Generate Root Morphemes
makeRootMorphemes :: MeaningData -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> (Int, Int) -> [(LexCat, Int, Int, Int, Int)] -> RVar [Morpheme]
makeRootMorphemes mData onsets nucs codas tones set numPerLC = do
  foo <- mapM (\(lc,_,_,_,sr) -> makeRootMorphemes_ mData lc sr onsets nucs codas tones set) ((Adpo,0,0,0,0) : numPerLC)
  return $ concat foo

makeRootMorphemes_ :: MeaningData -> LexCat -> Int -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> (Int, Int) -> RVar [Morpheme]
makeRootMorphemes_ mData Noun sr onsets nucs codas tones set = makeRootMorphemes__ Noun (inputNouns mData) sr onsets nucs codas tones set
makeRootMorphemes_ mData Verb sr onsets nucs codas tones set = makeRootMorphemes__ Verb (inputVerbs mData) sr onsets nucs codas tones set
makeRootMorphemes_ mData Adj sr onsets nucs codas tones set = makeRootMorphemes__ Adj (inputAdjs mData) sr onsets nucs codas tones set
makeRootMorphemes_ mData Adpo sr onsets nucs codas tones set = makeRootMorphemes__ Adpo (inputAdpos mData) sr onsets nucs codas tones set
makeRootMorphemes_ _ _ _ _ _ _ _ _ = return []

makeRootMorphemes__ :: LexCat -> [Text] -> Int -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> (Int, Int) -> RVar [Morpheme]
makeRootMorphemes__ lc foo 0 onsets nucs codas tones set = mapM (\i -> MorphemeS (Meaning lc i) Root <$> makeMorpheme onsets nucs codas tones set) foo
makeRootMorphemes__ lc foo _ onsets _ _ _ set = mapM (\i -> ConsonantalRoot (Meaning lc i) Root <$> makeConsonantalRoot onsets set) foo

-- Generate a root morpheme given vowels, consonant clusters, and some settings
makeMorpheme :: [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> (Int, Int) -> RVar [Syllable]
makeMorpheme onsets nucs codas tones (ns,xs) = do
  -- decide how many syllables in the morpheme
  s <- triangle ns xs
  syllables <- replicateM s (makeRootSyllable onsets nucs codas tones)

  -- assign stress
  assignStress syllables

makeRootSyllable :: [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> RVar Syllable
makeRootSyllable onsets nucs codas tones = do
  onset  <- fromMaybe [] <$> yuleSimonChoice 1 onsets
  nuclei <- fromMaybe Blank <$> yuleSimonChoice 1 nucs
  coda   <- fromMaybe [] <$> yuleSimonChoice 1 codas
  tone   <- choice tones
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

-- Generate Semitic Root
makeConsonantalRoot :: [ConsCluster] -> (Int, Int) -> RVar [[Phoneme]]
makeConsonantalRoot conclusts (ns, xs) = do
  r <- triangle (max 1 (ns-1)) (xs-1)
  replicateM r (fromMaybe [] <$> yuleSimonChoice 1 conclusts) -- should be hyperparameter

-- Generate Inflectional Morphemes
makeInflectionMorphemes :: [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> InflectionMap -> (LexCat, Int, Int, Int, Int) -> (Int, Int) -> RVar [Morpheme]
makeInflectionMorphemes onsets nucs codas tones inflMap (lc, i, j, k, l) set = do
  part <- makeExponentSystems lc Particle i onsets nucs codas tones inflMap set
  pref <- makeExponentSystems lc Prefix j onsets nucs codas tones inflMap set
  suff <- makeExponentSystems lc Suffix k onsets nucs codas tones inflMap set
  trans <- makeExponentSystems lc Transfix l onsets nucs codas tones inflMap set
  return (part ++ pref ++ suff ++ trans)

makeExponentSystems :: LexCat -> MorphType -> Int -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> InflectionMap -> (Int, Int) -> RVar [Morpheme]
makeExponentSystems _ _ 0 _ _ _ _ _ _ = return []
makeExponentSystems lc morphType i onsets nucs codas tones inflMap set = (++) <$> makeExponentSystems lc morphType (i-1) onsets nucs codas tones inflMap set <*> makeExponentSystem lc morphType i onsets nucs codas tones inflMap set

makeExponentSystem :: LexCat -> MorphType -> Int -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> InflectionMap -> (Int, Int) -> RVar [Morpheme]
makeExponentSystem lc Transfix i onsets nucs codas tones inflMap set = do
  let combos = makeCombos $ cleanInflectionSys inflMap lc Transfix i
  roots <- replicateM (length combos) (makeTransfix onsets nucs codas tones set)
  let morphs = zipWith (\x y -> PatternMorph x Transfix y) (InflMeaning lc <$> combos) roots
  return morphs
makeExponentSystem lc morphType i onsets nucs codas tones inflMap (ns,xs) = do
  let combos = makeCombos $ cleanInflectionSys inflMap lc morphType i
  roots <- replicateM (length combos) (makeMorpheme onsets nucs codas tones (ns,ns)) -- pick minimum
  let morphs = zipWith (\x y -> MorphemeS x morphType y) (InflMeaning lc <$> combos) roots
  return morphs

-- For a given LexCat, MorphType, and Int, return the grammatical categories expressed there
-- Based on the InflectionMap
cleanInflectionSys :: InflectionMap -> LexCat -> MorphType -> Int -> GramCatExpresses
-- ([Express Gender], [Express Animacy], [Express Case], [Express Number], [Express Definiteness], [Express Specificity], [Express Topic], [Express Person], [Express Honorific], [Express Polarity], [Express Tense], [Express Aspect], [Express Mood], [Express Voice], [Express Evidentiality], [Express Transitivity], [Express Volition])
cleanInflectionSys inflMap lc mt i = GramCatExpresses gens anis cass nums defs spes tops pers hons pols tens asps moos vois evis tras vols where
  gens = cleanSys (getGenSys inflMap) lc mt i
  anis = cleanSys (getAniSys inflMap) lc mt i
  cass = cleanSys (getCasSys inflMap) lc mt i
  nums = cleanSys (getNumSys inflMap) lc mt i
  defs = cleanSys (getDefSys inflMap) lc mt i
  spes = cleanSys (getSpeSys inflMap) lc mt i
  tops = cleanSys (getTopSys inflMap) lc mt i
  pers = cleanSys (getPerSys inflMap) lc mt i
  hons = cleanSys (getHonSys inflMap) lc mt i
  pols = cleanSys (getPolSys inflMap) lc mt i
  tens = cleanSys (getTenSys inflMap) lc mt i
  asps = cleanSys (getAspSys inflMap) lc mt i
  moos = cleanSys (getMooSys inflMap) lc mt i
  vois = cleanSys (getVoiSys inflMap) lc mt i
  evis = cleanSys (getEviSys inflMap) lc mt i
  tras = cleanSys (getTraSys inflMap) lc mt i
  vols = cleanSys (getVolSys inflMap) lc mt i

  cleanSys :: GramCat a => Manifest a -> LexCat -> MorphType -> Int -> [Express a]
  cleanSys NoManifest _ _ _ = [NoExpress]
  cleanSys (Manifest t x) lc mt i = out where
    filt = filter (== (lc, mt, i)) t
    out
      | null filt = [NoExpress]
      | otherwise = map Express x

makeCombos :: GramCatExpresses -> [GramCatExpress]
makeCombos (GramCatExpresses gens anis cass nums defs spes tops pers hons pols tens asps moos vois evis tras vols) = GramCatExpress <$> gens <*> anis <*> cass <*> nums <*> defs <*> spes <*> tops <*> pers <*> hons <*> pols <*> tens <*> asps <*> moos <*> vois <*> evis <*> tras <*> vols

-- Generate Transfix, which go between the consonants of a Consonantal Root
makeTransfix :: [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> (Int, Int) -> RVar [Syllable]
makeTransfix onsets nucs codas tones (ns,xs) = do
  p <- triangle (max ns 2) xs
  patterns <- replicateM p (makeTransfix_ onsets nucs codas tones)
  assignStress patterns

makeTransfix_ :: [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> RVar Syllable
makeTransfix_ onsets nucs codas tones = do
  --onset <- fromMaybe [] <$> yuleSimonChoice 1 onsets
  nuclei <- fromMaybe Blank <$> yuleSimonChoice 1 nucs
  --coda <- fromMaybe [] <$> yuleSimonChoice 1 codas
  tone   <- choice tones
  return $ Syllable [] nuclei [] tone NONES

-- This picks the inflectional morphemes for the dictionary form of words
pickLemmaMorphemes :: InflectionMap -> [Morpheme] -> LexCat -> RVar [Morpheme]
pickLemmaMorphemes inflSys inflMorphs lc = do
  -- Generate a random AllExpress for this LexCat
  lemmaExpress <- generateInflection inflSys lc
  -- Filter the master list of inflection Morpheme's, this LexCat only
  let relMorphs = filter (\x -> lc == getLC (getMeaning x)) inflMorphs
  -- Filter down to the ones that help satisfy the lemmaExpress
  let lemmaMorphs = filter (\x -> compareInfl (getAllExpress $ getMeaning x) lemmaExpress) relMorphs
  return lemmaMorphs


-- Derivational Morphology
makeDerivationMorphemes :: MeaningData -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> (Int, Int) -> RVar [Morpheme]
makeDerivationMorphemes mData onsets nucs codas tones set = mapM (\d -> MorphemeS d Suffix <$> makeMorpheme onsets nucs codas tones set) (inputDerivs mData)
