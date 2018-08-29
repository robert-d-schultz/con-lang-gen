module Gen.Morphology
( makeLexicalInflection
, cleanInflectionSys
) where

import ClassyPrelude hiding (Word)
import Data.RVar

import Data.Phoneme
import Data.Inflection
import Data.Other

import Gen.Root

-- Inflections for each lexical category
makeLexicalInflection :: [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> InflectionMap -> (LexCat, Int, Int, Int) -> RVar [ManifestSystem]
makeLexicalInflection onsets nucs codas tones inflMap (lc, i, j, k) = do
  part <- makeParticleSystems lc i onsets nucs codas tones inflMap
  pref <- makePrefixSystems lc j onsets nucs codas tones inflMap
  suff <- makeSuffixSystems lc k onsets nucs codas tones inflMap
  return (part ++ pref ++ suff)

-- Prefixs
makePrefixSystems :: LexCat -> Int -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> InflectionMap -> RVar [ManifestSystem]
makePrefixSystems _ 0 _ _ _ _ _ = return []
makePrefixSystems lc i onsets nucs codas tones gramSys = (++) <$> makePrefixSystems lc (i-1) onsets nucs codas tones gramSys <*> ((:[]) <$> makePrefixSystem lc i onsets nucs codas tones gramSys)

makePrefixSystem :: LexCat -> Int -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> InflectionMap -> RVar ManifestSystem
makePrefixSystem lc i onsets nucs codas tones gramSys = do
  let (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanInflectionSys gramSys lc Prefix i
  let combos = (,,,,,,,,,,,,,,,,) <$> gen <*> ani <*> cas <*> num <*> def <*> spe <*> top <*> per <*> hon <*> pol <*> ten <*> asp <*> moo <*> voi <*> evi <*> tra <*> vol
  morphs <- replicateM (length combos) (makeRoot onsets nucs codas tones (1, 1))
  return $ ManifestSystem lc Prefix (zip morphs combos)

-- Suffixs
makeSuffixSystems :: LexCat -> Int -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> InflectionMap -> RVar [ManifestSystem]
makeSuffixSystems _ 0 _ _ _ _ _ = return []
makeSuffixSystems lc i onsets nucs codas tones gramSys = (++) <$> makeSuffixSystems lc (i-1) onsets nucs codas tones gramSys <*> ((:[]) <$> makeSuffixSystem lc i onsets nucs codas tones gramSys)

makeSuffixSystem :: LexCat -> Int -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> InflectionMap -> RVar ManifestSystem
makeSuffixSystem lc i onsets nucs codas tones gramSys = do
  let (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanInflectionSys gramSys lc Suffix i
  let combos = (,,,,,,,,,,,,,,,,) <$> gen <*> ani <*> cas <*> num <*> def <*> spe <*> top <*> per <*> hon <*> pol <*> ten <*> asp <*> moo <*> voi <*> evi <*> tra <*> vol
  morphs <- replicateM (length combos) (makeRoot onsets nucs codas tones (1, 1))
  return $ ManifestSystem lc Suffix (zip morphs combos)

-- Particles
makeParticleSystems :: LexCat -> Int -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> InflectionMap -> RVar [ManifestSystem]
makeParticleSystems _ 0 _ _ _ _ _ = return []
makeParticleSystems lc i onsets nucs codas tones gramSys = (++) <$> makeParticleSystems lc (i-1) onsets nucs codas tones gramSys <*> ((:[]) <$> makeParticleSystem lc i onsets nucs codas tones gramSys)

makeParticleSystem :: LexCat -> Int -> [ConsCluster] -> [Phoneme] -> [ConsCluster] -> [Tone] -> InflectionMap -> RVar ManifestSystem
makeParticleSystem lc i onsets nucs codas tones gramSys = do
  let (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanInflectionSys gramSys lc Particle i
  let combos = (,,,,,,,,,,,,,,,,) <$> gen <*> ani <*> cas <*> num <*> def <*> spe <*> top <*> per <*> hon <*> pol <*> ten <*> asp <*> moo <*> voi <*> evi <*> tra <*> vol
  morphs <- replicateM (length combos) (makeRoot onsets nucs codas tones (1, 2))
  return $ ManifestSystem lc Particle (zip morphs combos)

-- Clean sys
cleanSys :: Manifest a -> LexCat -> ManifestType -> Int -> [Express a]
cleanSys NoManifest _ _ _ = [NoExpress]
cleanSys (Manifest t x) lc mt i = out where
  filt = filter (== (lc, mt, i)) t
  out
    | null filt = [NoExpress]
    | otherwise = map Express x

cleanInflectionSys :: InflectionMap -> LexCat -> ManifestType -> Int -> ([Express Gender], [Express Animacy], [Express Case], [Express Number], [Express Definiteness], [Express Specificity], [Express Topic], [Express Person], [Express Honorific], [Express Polarity], [Express Tense], [Express Aspect], [Express Mood], [Express Voice], [Express Evidentiality], [Express Transitivity], [Express Volition])
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
