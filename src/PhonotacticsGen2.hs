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

-- make sonority hierarchy (consonants only)
makeSonHier :: [Phone] -> RVar [[Phone]]
makeSonHier cons = filter (not.null) . concat <$> sequence [glides, liquids, nasals, obstruents] where
  (g, cons2) = partition (\x -> manner x == APPROXIMANT && place x >= ALVEOLOPALATAL) cons
  (l, cons3) = partition (\x -> manner x >= APPROXIMANT) cons2
  (n, o) = partition (\x -> manner x == NASAL) cons3
  glides = furtherDivide g []
  liquids = furtherDivide l []
  nasals = furtherDivide n []
  obstruents = furtherDivide o []

-- further divides the glide, liquid, nasal, and obstruent groups
furtherDivide :: [Phone] -> [[Phone]] -> RVar [[Phone]]
furtherDivide [] out = return out
furtherDivide cons out = do
  len <- uniform 1 (length cons)
  let (foo, bar) = splitAt len cons
  furtherDivide bar (foo:out)

retrieveSon :: [[Phone]] -> Phone -> Int
retrieveSon sonHier Vowel{} = length sonHier + 1
retrieveSon sonhier phone = fromJust (elemIndex True $ map (elem phone) (reverse sonhier))


-- how it's gonna work:
-- decide beforehand the syllabify rules
-- 1. vowel = syllable nucleus
-- 2. all consonants in descending sonority before nucleus = syllable onset
-- 3. (unlikely optional) all consonants in descending sonority after nucleus = syllable coda
-- 4. (optional) disallowed adjacencies?
-- hierarchy: vowels > glides > liquids > nasals > obstruents
-- make a specific hierarchy based on the above?
-- decide max consonants word initial
-- decide max consonants word final
-- decide max consonants intervocalic?
-- ccvcccvccccvcvcvc
-- generates words as ([c], [(v,[c])])?
