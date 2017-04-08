module Gen.Phonotactics
( makeSonHier
, makeSonHier_
, retrieveSon
, makeOnsets
, makeCodas
) where

import Control.Exception

import Control.Monad
import Data.List
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Data.Maybe

import Data.Phoneme

-- Make sonority hierarchy (consonants only)
makeSonHier :: [Phoneme] -> RVar (Int, [[Phoneme]])
makeSonHier cons = do
  let (glides, cons2) = partition (\x -> cmanner x == APPROXIMANT && cplace x > DORSAL) cons
  let (liquids, cons3) = partition (\x -> cmanner x `elem` [APPROXIMANT, LAPPROXIMANT, TRILL, FLAP, LFLAP]) cons2
  let (nasals, cons4) = partition (\x -> cmanner x == NASAL) cons3
  let (af, cons5) = partition (\x -> cmanner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && cvoice x == ASPIRATED) cons4
  let (mf, cons6) = partition (\x -> cmanner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && cvoice x == MODAL) cons5
  let (sf, cons7) = partition (\x -> cmanner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && cvoice x `elem` [SLACK, STIFF]) cons6
  let (bf, cons8) = partition (\x -> cmanner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && cvoice x `elem` [BREATHY, CREAKY]) cons7
  let (vf, cons9) = partition (\x -> cmanner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && cvoice x == VOICELESS) cons8
  let (aa, cons10) = partition (\x -> cmanner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && cvoice x == ASPIRATED) cons9
  let (ma, cons11) = partition (\x -> cmanner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && cvoice x == MODAL) cons10
  let (sa, cons12) = partition (\x -> cmanner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && cvoice x `elem` [SLACK, STIFF]) cons11
  let (ba, cons13) = partition (\x -> cmanner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && cvoice x `elem` [BREATHY, CREAKY]) cons12
  let (va, cons14) = partition (\x -> cmanner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && cvoice x == VOICELESS) cons13
  let (as, cons15) = partition (\x -> cmanner x == STOP && cvoice x == ASPIRATED) cons14
  let (ms, cons16) = partition (\x -> cmanner x == STOP && cvoice x == MODAL) cons15
  let (ss, cons17) = partition (\x -> cmanner x == STOP && cvoice x `elem` [SLACK, STIFF]) cons16
  let (bs, cons18) = partition (\x -> cmanner x == STOP && cvoice x `elem` [BREATHY, CREAKY]) cons17
  let (vs, other) = partition (\x -> cmanner x == STOP && cvoice x == VOICELESS) cons18

  let aobstruent = concat [af, aa, as]
  let mobstruent = concat [mf, ma, ms]
  let sobstruent = concat [sf, sa, ss]
  let bobstruent = concat [bf, ba, bs]
  let vobstruent = concat [vf, va, vs]
  let fricatives = concat [af, mf, sf, bf, vf]
  let affricates = concat [aa, ma, sa, ba, va]
  let stops = concat [as, ms, ss, bs, vs]
  let obstruents = concat [fricatives, affricates, stops]

  scheme <- choice [ (1, [glides, liquids, nasals, obstruents, other])
                   , (2, [glides, liquids, nasals, fricatives, affricates, stops, other])
                   , (3, [glides, liquids, nasals, aobstruent, mobstruent, sobstruent, bobstruent, vobstruent, other])
                   , (4, [glides, liquids, nasals, af, mf, sf, bf, vf, aa, ma, sa, ba, va, as, ms, ss, bs, vs, other])
                   ]
  return (fst scheme, filter (not.null) (snd scheme))

-- make sonHier from scheme number
makeSonHier_ :: [Phoneme] -> Int -> [[Phoneme]]
makeSonHier_ cons i = filter (not.null) scheme where
  (glides, cons2) = partition (\x -> cmanner x == APPROXIMANT && cplace x > DORSAL) cons
  (liquids, cons3) = partition (\x -> cmanner x `elem` [APPROXIMANT, LAPPROXIMANT, TRILL, FLAP, LFLAP]) cons2
  (nasals, cons4) = partition (\x -> cmanner x == NASAL) cons3
  (af, cons5) = partition (\x -> cmanner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && cvoice x == ASPIRATED) cons4
  (mf, cons6) = partition (\x -> cmanner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && cvoice x == MODAL) cons5
  (sf, cons7) = partition (\x -> cmanner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && cvoice x `elem` [SLACK, STIFF]) cons6
  (bf, cons8) = partition (\x -> cmanner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && cvoice x `elem` [BREATHY, CREAKY]) cons7
  (vf, cons9) = partition (\x -> cmanner x `elem` [LFRICATIVE, FRICATIVE, SILIBANT] && cvoice x == VOICELESS) cons8
  (aa, cons10) = partition (\x -> cmanner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && cvoice x == ASPIRATED) cons9
  (ma, cons11) = partition (\x -> cmanner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && cvoice x == MODAL) cons10
  (sa, cons12) = partition (\x -> cmanner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && cvoice x `elem` [SLACK, STIFF]) cons11
  (ba, cons13) = partition (\x -> cmanner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && cvoice x `elem` [BREATHY, CREAKY]) cons12
  (va, cons14) = partition (\x -> cmanner x `elem` [LAFFRICATE, AFFRICATE, SAFFRICATE] && cvoice x == VOICELESS) cons13
  (as, cons15) = partition (\x -> cmanner x == STOP && cvoice x == ASPIRATED) cons14
  (ms, cons16) = partition (\x -> cmanner x == STOP && cvoice x == MODAL) cons15
  (ss, cons17) = partition (\x -> cmanner x == STOP && cvoice x `elem` [SLACK, STIFF]) cons16
  (bs, cons18) = partition (\x -> cmanner x == STOP && cvoice x `elem` [BREATHY, CREAKY]) cons17
  (vs, other) = partition (\x -> cmanner x == STOP && cvoice x == VOICELESS) cons18

  aobstruent = concat [af, aa, as]
  mobstruent = concat [mf, ma, ms]
  sobstruent = concat [sf, sa, ss]
  bobstruent = concat [bf, ba, bs]
  vobstruent = concat [vf, va, vs]
  fricatives = concat [af, mf, sf, bf, vf]
  affricates = concat [aa, ma, sa, ba, va]
  stops = concat [as, ms, ss, bs, vs]
  obstruents = concat [fricatives, affricates, stops]

  scheme | i == 1 = [glides, liquids, nasals, obstruents, other]
         | i == 2 = [glides, liquids, nasals, fricatives, affricates, stops, other]
         | i == 3 = [glides, liquids, nasals, aobstruent, mobstruent, sobstruent, bobstruent, vobstruent, other]
         | i == 4 = [glides, liquids, nasals, af, mf, sf, bf, vf, aa, ma, sa, ba, va, as, ms, ss, bs, vs, other]

-- Further divides the glide, liquid, nasal, and obstruent groups
-- Not used
furtherDivide :: [Phoneme] -> [[Phoneme]] -> RVar [[Phoneme]]
furtherDivide [] out = return out
furtherDivide cons out = do
  len <- uniform 1 (length cons)
  let (foo, bar) = splitAt len cons
  furtherDivide bar (foo:out)

-- Retrieve the sonority level of a given phone
retrieveSon :: [[Phoneme]] -> Phoneme -> Int
retrieveSon sonHier Vowel{} = length sonHier + 1
retrieveSon sonHier Diphthong{} = length sonHier + 1
retrieveSon sonHier Blank = 0
retrieveSon sonhier phone = fromJust (elemIndex True $ map (elem phone) (reverse sonhier))

-- Generate valid onsets
makeOnsets :: [[Phoneme]] -> (Int, Int) -> RVar [[Phoneme]]
makeOnsets sonHier (no, xo) = do
  n <- uniform 5 10
  replicateM n $ join (makeConsonantCluster <$> uniform no xo <*> return (reverse sonHier) <*> return [])

-- Generate valid codas
makeCodas :: [[Phoneme]] -> (Int, Int) -> RVar [[Phoneme]]
makeCodas sonHier (nc, xc) = do
  n <- uniform 5 10
  replicateM n $ join (makeConsonantCluster <$> uniform nc xc <*> return sonHier <*> return [])

-- Generate a consonant cluster
-- Goes through sonority hierarchy taking consonants from each grouping
makeConsonantCluster :: Int -> [[Phoneme]] -> [Phoneme] -> RVar [Phoneme]
makeConsonantCluster 0 _ out = return out
makeConsonantCluster _ [] out = return out
makeConsonantCluster l sonhier out = do
  i <- uniform 0 1
  newCs <- sample i (head sonhier)
  makeConsonantCluster (l-i) (tail sonhier) (out ++ newCs)
