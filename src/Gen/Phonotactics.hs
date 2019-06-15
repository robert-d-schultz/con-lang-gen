module Gen.Phonotactics
( makeSonHier
, retrieveSon
, makeOnsets
, makeNuclei
, makeCodas
) where

import ClassyPrelude hiding ((\\))

import Data.List (elemIndex, (\\))

import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)

import Data.Phoneme
import Data.Word

import HelperFunctions

-- Plan:
-- Make sonority hierarchies be rule-based
-- Rules will be procedurally generated, and recorded per-language
-- This will allow the hierarchy to be reconstructed when new phonemes are introduced

-- Question:
-- What is the relation between the sonority hierarchy and valid consonant clusters?
-- Are they the exact same ie. all clusters that the can come out of the hierarchy are valid?
-- Well, no: /s/ can violate it in some languages (and not in others)
-- Also, valid English CC's are (seemingly) subset of all possible ones (/dv/, /vl/, /tl/ are "bad" and from loans)

-- Constraints/Rules:
-- Minimum Sonority Distance
-- Like if the SH is glide-liquid-nasal-fricative-stop
-- and the MSD is 2, you can't have stop-fricative onsets
-- Some languages allow 0 (stop-stop is valid, for instance)

-- Identical Place of Articulation
-- Apparently having the same POA in consecutive consonants (in a syllable onset) is bad?


-- Aside:
-- The available consonant clusters can effect sound changes
-- Like if a consonant changes in a particular word, and it results in a bad CC
-- then the consonant can instead be deleted, or a vowel can instead be inserted
-- On the other hand, the sound changes can effect the available consonant clusters
-- Like in the above example, the resultant "bad" CC can be added in as an available one

-- In either case there needs to be a function that runs over a recently changed lexicon and looks for these bad CCs
-- Then it can either decide to fix them or add them


-- Generate valid onsets
makeOnsets :: [[Phoneme]] -> (Int, Int) -> RVar [[Phoneme]]
makeOnsets sonHier (msd, maxOnsetC) = do
  n <- uniform 5 10
  replicateM n $ makeConsonantCluster (msd, maxOnsetC) sonHier []

-- Pick valid nuclei
makeNuclei :: [Phoneme] -> [[Phoneme]] -> RVar [Phoneme]
makeNuclei vows sonHier = do
  -- testing out syllabic consonants
  -- take the most sonorous consonants (usually glides) and picks one to be a nucleus
  syllabicCs <- fromMaybe (return []) $ join (safeSample 0 <$> lastMay sonHier)
  return $ vows ++ syllabicCs

-- Generate valid codas
makeCodas :: Int -> Int -> [ConsCluster] -> [ConsCluster] -> [Phoneme] -> [[Phoneme]] -> (Int, Int) -> RVar [[Phoneme]]
makeCodas 0 _ codas onsets cns _ _ = do
  let unused = cns \\ concat (codas ++ onsets)
  return $ codas ++ map (:[]) unused -- makes sure all consonants are used somewhere
makeCodas _ 50 codas onsets cns sonHier settings = makeCodas 0 50 codas onsets cns sonHier settings -- trace "Failed to generate ALL codaCCs" $
makeCodas i limit [] onsets cns sonHier settings = do
  m <- uniform 0 (length cns) -- number of coda "singletons" (not counting possibly unused consonants)
  codas <- fromMaybe (return []) $ safeSample m cns
  makeCodas i limit ([] : map (:[]) codas) onsets cns sonHier settings
makeCodas i limit codas onsets cns sonHier settings = do
  newCC <- makeConsonantCluster settings (reverse sonHier) []
  if checkCoda onsets codas newCC
    then makeCodas (i-1) (limit+1) (newCC:codas) onsets cns sonHier settings
    else makeCodas i (limit+1) codas onsets cns sonHier settings

checkCoda :: [ConsCluster] -> [ConsCluster] -> [Phoneme] -> Bool
checkCoda onsets codas coda = length coda > 1
                           && coda `notElem` codas
                           && checkCoda2 coda
                           && checkCoda3 onsets codas coda

-- checks for identical consecutive consonants
checkCoda2 :: [Phoneme] -> Bool
checkCoda2 [] = True
checkCoda2 (p:ps) = fromMaybe True ((p /=) <$> headMay ps) && checkCoda2 ps

-- at every splitAt of a coda, if the right side is an onset, then the left side MUST be an existing coda
checkCoda3 :: [ConsCluster] -> [ConsCluster] -> [Phoneme] -> Bool
checkCoda3 _ _ [] = True
checkCoda3 onsets codas coda = all (\(c,o) -> (o `elem` onsets) <= (c `elem` codas)) everySplit where
  everySplit = map (`splitAt` coda) [0..length coda-1]

-- Generate a consonant cluster
-- msd = Minimum Sonority Distance
makeConsonantCluster :: (Int, Int) -> [[Phoneme]] -> [Phoneme] -> RVar [Phoneme]
makeConsonantCluster _ [] out = return out
makeConsonantCluster (_, 0) _ out = return out
makeConsonantCluster (msd, maxC) sonHier out = do
  (theRest, newC) <- maybe (return ([],[])) (fmap $ second (:[])) (join $ choiceExtract <$> headMay sonHier)
  let sonHierN = theRest : fromMaybe [] (tailMay sonHier)
  join $ choice [ makeConsonantCluster (msd, maxC-1) (drop msd sonHierN) (out ++ newC)
                , makeConsonantCluster (msd, maxC) (drop 1 sonHierN) out
                ]

-- Working definition of Semivowel
isSemiVowel :: Phoneme -> Bool
isSemiVowel (Consonant (COARTICULATED p1 p2) m h a)
  | p1 /= BILABIAL = False
  | p2 < PALATAL = False
  | m /= APPROXIMANT = False
  | otherwise = True
isSemiVowel (Consonant p m h a)
  | p < PALATAL = False
  | m /= APPROXIMANT = False
  | otherwise = True
isSemiVowel _ = False

-- Make sonority hierarchy from consonant inventory and scheme number
-- The hierarchies get more granular as scheme number increases
makeSonHier :: [Phoneme] -> Int -> [[Phoneme]]
makeSonHier cns i = filter (not.null) scheme where
  (glides, cons2) = partition isSemiVowel cns
  (liquids, cons3) = partition (\x -> getManner x `elem` [APPROXIMANT, LAPPROXIMANT, TRILL, FLAP, LFLAP]) cons2
  (nasals, cons4) = partition (\x -> getManner x == NASAL) cons3
  (af, cons5) = partition (\x -> getManner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && getVoice x == ASPIRATED) cons4
  (mf, cons6) = partition (\x -> getManner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && getVoice x == MODAL) cons5
  (sf, cons7) = partition (\x -> getManner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && getVoice x `elem` [SLACK, STIFF]) cons6
  (bf, cons8) = partition (\x -> getManner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && getVoice x `elem` [BREATHY, CREAKY]) cons7
  (vf, cons9) = partition (\x -> getManner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && getVoice x == VOICELESS) cons8
  (aa, cons10) = partition (\x -> getManner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && getVoice x == ASPIRATED) cons9
  (ma, cons11) = partition (\x -> getManner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && getVoice x == MODAL) cons10
  (sa, cons12) = partition (\x -> getManner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && getVoice x `elem` [SLACK, STIFF]) cons11
  (ba, cons13) = partition (\x -> getManner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && getVoice x `elem` [BREATHY, CREAKY]) cons12
  (va, cons14) = partition (\x -> getManner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && getVoice x == VOICELESS) cons13
  (as, cons15) = partition (\x -> getManner x == STOP && getVoice x == ASPIRATED) cons14
  (ms, cons16) = partition (\x -> getManner x == STOP && getVoice x == MODAL) cons15
  (ss, cons17) = partition (\x -> getManner x == STOP && getVoice x `elem` [SLACK, STIFF]) cons16
  (bs, cons18) = partition (\x -> getManner x == STOP && getVoice x `elem` [BREATHY, CREAKY]) cons17
  (vs, other) = partition (\x -> getManner x == STOP && getVoice x == VOICELESS) cons18 -- other should always be empty

  sspViolatingS = filter (\x -> getPlace x `elem` [LAMINALALVEOLAR, APICOALVEOLAR, PALATOALVEOLAR, APICALRETROFLEX] && getManner x == SILIBANT && getVoice x == VOICELESS) cns

  aobstruent = concat [af, aa, as]
  mobstruent = concat [mf, ma, ms]
  sobstruent = concat [sf, sa, ss]
  bobstruent = concat [bf, ba, bs]
  vobstruent = concat [vf, va, vs]
  fricatives = concat [af, mf, sf, bf, vf]
  affricates = concat [aa, ma, sa, ba, va]
  stops = concat [as, ms, ss, bs, vs]
  obstruents = concat [fricatives, affricates, stops]

  scheme | i == 1 = filter (not . null) [other, obstruents, nasals, liquids, glides] -- most general
         | i == 2 = filter (not . null) [other, stops, affricates, fricatives, nasals, liquids, glides] -- seperate obstruents by manner
         | i == 3 = filter (not . null) [other, vobstruent, bobstruent, sobstruent, mobstruent, aobstruent, nasals, liquids, glides] -- seperate obstruents by phonation
         | i == 4 = filter (not . null) [other, vs, bs, ss, ms, as, va, ba, sa, ma, aa, vf, bf, sf, mf, af, nasals, liquids, glides] -- seperate by both
         | i == 5 = filter (not . null) $ sspViolatingS : [other, vs, bs, ss, ms, as, va, ba, sa, ma, aa, vf, bf, sf, mf, af, nasals, liquids, glides] -- SSP-violating S
         | otherwise = []

-- Retrieve the sonority level of a given phoneme
-- The greater the number, the closer to "Vowel"
retrieveSon :: [[Phoneme]] -> Phoneme -> Int
retrieveSon sonHier Vowel{} = length sonHier + 1
retrieveSon sonHier Diphthong{} = length sonHier + 1
retrieveSon _ Blank = 0
retrieveSon sonhier p = fromMaybe 0 (elemIndex True $ map (elem p) sonhier)
