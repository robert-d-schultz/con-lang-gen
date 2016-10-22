module PhonotacticsGen2
( makeSonHier
, retrieveSon
) where

import Data.List
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Data.Maybe

import PhonemeType2

-- Make sonority hierarchy (consonants only)
makeSonHier :: [Phoneme] -> RVar [[Phoneme]]
makeSonHier cons = filter (not.null) . concat <$> sequence [glides, liquids, nasals, obstruents] where
  (g, cons2) = partition (\x -> cmanner x == APPROXIMANT && cplace x >= ALVEOLOPALATAL) cons
  (l, cons3) = partition (\x -> cmanner x `elem` [APPROXIMANT, LAPPROXIMANT, TRILL, FLAP, LFLAP]) cons2
  (n, o) = partition (\x -> cmanner x == NASAL) cons3
  glides = furtherDivide g []
  liquids = furtherDivide l []
  nasals = furtherDivide n []
  obstruents = furtherDivide o []

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
