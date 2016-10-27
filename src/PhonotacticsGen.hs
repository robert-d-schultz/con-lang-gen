module PhonotacticsGen
( makeSonHier
, retrieveSon
) where

import Data.List
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Data.Maybe

import PhonemeData

-- Make sonority hierarchy (consonants only)
makeSonHier :: [Phoneme] -> RVar [[Phoneme]]
makeSonHier cons = filter (not.null) <$> scheme where
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
  scheme = choice [ [glides, liquids, nasals, obstruents, other]
                  , [glides, liquids, nasals, fricatives, affricates, stops, other]
                  , [glides, liquids, nasals, aobstruent, mobstruent, sobstruent, bobstruent, vobstruent, other]
                  , [glides, liquids, nasals, af, mf, sf, bf, vf, aa, ma, sa, ba, va, as, ms, ss, bs, vs, other]
                  ]
  aobstruent = concat [af, aa, as]
  mobstruent = concat [mf, ma, ms]
  sobstruent = concat [sf, sa, ss]
  bobstruent = concat [bf, ba, bs]
  vobstruent = concat [vf, va, vs]
  fricatives = concat [af, mf, sf, bf, vf]
  affricates = concat [aa, ma, sa, ba, va]
  stops = concat [as, ms, ss, bs, vs]
  obstruents = concat [fricatives, affricates, stops]

-- Further divides the glide, liquid, nasal, and obstruent groups
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
retrieveSon sonhier phone = fromJust (elemIndex True $ map (elem phone) (reverse sonhier))
