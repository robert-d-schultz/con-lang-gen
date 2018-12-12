module Gen.Morphology
( makeLexicalInflection
, cleanInflectionSys
) where

import ClassyPrelude hiding (Word)
import Data.RVar

import Data.Phoneme
import Data.Word
import Data.Inflection
import Data.Other

import Gen.Root

-- Inflections for each lexical category
makeLexicalInflection :: [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> InflectionMap -> (LexCat, Int, Int, Int) -> RVar [Morpheme]
makeLexicalInflection onsets nucs codas tones inflMap (lc, i, j, k) = do
  part <- makeExponentSystems lc Particle i onsets nucs codas tones inflMap
  pref <- makeExponentSystems lc Prefix j onsets nucs codas tones inflMap
  suff <- makeExponentSystems lc Suffix k onsets nucs codas tones inflMap
  return (part ++ pref ++ suff)

makeExponentSystems :: LexCat -> InflType -> Int -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> InflectionMap -> RVar [Morpheme]
makeExponentSystems _ _ 0 _ _ _ _ _ = return []
makeExponentSystems lc inflType i onsets nucs codas tones gramSys = (++) <$> makeExponentSystems lc inflType (i-1) onsets nucs codas tones gramSys <*> makeExponentSystem lc inflType i onsets nucs codas tones gramSys

makeExponentSystem :: LexCat -> InflType -> Int -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> InflectionMap -> RVar [Morpheme]
makeExponentSystem lc inflType i onsets nucs codas tones gramSys = do
  let (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanInflectionSys gramSys lc inflType i
  let combos = (,,,,,,,,,,,,,,,,) <$> gen <*> ani <*> cas <*> num <*> def <*> spe <*> top <*> per <*> hon <*> pol <*> ten <*> asp <*> moo <*> voi <*> evi <*> tra <*> vol
  roots <- replicateM (length combos) (makeRoot onsets nucs codas tones (1, 1))
  let morphs = zipWith MorphemeS (InflMeaning lc inflType <$> combos) roots
  return morphs

-- For a given LexCat, InflType, and Int, return the grammatical categories expressed there
-- Based on the InflectionMap
cleanInflectionSys :: InflectionMap -> LexCat -> InflType -> Int -> ([Express Gender], [Express Animacy], [Express Case], [Express Number], [Express Definiteness], [Express Specificity], [Express Topic], [Express Person], [Express Honorific], [Express Polarity], [Express Tense], [Express Aspect], [Express Mood], [Express Voice], [Express Evidentiality], [Express Transitivity], [Express Volition])
cleanInflectionSys inflMap lc mt i = (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) where
  gen = cleanSys (getGenSys inflMap) lc mt i
  ani = cleanSys (getAniSys inflMap) lc mt i
  cas = cleanSys (getCasSys inflMap) lc mt i
  num = cleanSys (getNumSys inflMap) lc mt i
  def = cleanSys (getDefSys inflMap) lc mt i
  spe = cleanSys (getSpeSys inflMap) lc mt i
  top = cleanSys (getTopSys inflMap) lc mt i
  per = cleanSys (getPerSys inflMap) lc mt i
  hon = cleanSys (getHonSys inflMap) lc mt i
  pol = cleanSys (getPolSys inflMap) lc mt i
  ten = cleanSys (getTenSys inflMap) lc mt i
  asp = cleanSys (getAspSys inflMap) lc mt i
  moo = cleanSys (getMooSys inflMap) lc mt i
  voi = cleanSys (getVoiSys inflMap) lc mt i
  evi = cleanSys (getEviSys inflMap) lc mt i
  tra = cleanSys (getTraSys inflMap) lc mt i
  vol = cleanSys (getVolSys inflMap) lc mt i

  cleanSys :: GramCat a => Manifest a -> LexCat -> InflType -> Int -> [Express a]
  cleanSys NoManifest _ _ _ = [NoExpress]
  cleanSys (Manifest t x) lc mt i = out where
    filt = filter (== (lc, mt, i)) t
    out
      | null filt = [NoExpress]
      | otherwise = map Express x
