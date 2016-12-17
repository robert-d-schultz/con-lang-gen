module Gen.Word
( makeDictionary
) where

import Prelude hiding (Word)
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Control.Monad
import Data.List

import LoadStuff
import Data.Phoneme
import Data.Other
import Data.Inflection
import Data.Grammar
import Gen.Phoneme
import Gen.Phonotactics

-- Generates the full lexicon from the root dictionary and inflections
makeDictionary :: [(LexCat, [ManifestSystem], [ManifestSystem], [ManifestSystem])] -> [((String, LexCat), Morpheme)] -> [((String, LexCat), Word)]
makeDictionary mans rootDict = concatMap (makeRootWord mans) rootDict
  -- words from root dictionary x exponents
  -- words from particles
  -- words from multiple roots?
  -- we'll get there

-- take inflection morphemes from ManifestSystem's
makeRootWord :: [(LexCat, [ManifestSystem], [ManifestSystem], [ManifestSystem])] -> ((String, LexCat), Morpheme) -> [((String, LexCat), Word)]
makeRootWord mans ((str, lc), morph)
  | null foo  = [((str,lc),Word [morph])]
  | otherwise = (,) <$> [(str,lc)] <*> map Word wrds where
  foo = filter (\(x,_,_,_) -> x == lc) mans
  (_,part,pref,suff) = head foo
  wrds = (\x y z -> [x,y,z]) <$> map fst (concatMap manSysCombos pref) <*> [morph] <*> map fst (concatMap manSysCombos suff)
