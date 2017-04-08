module Gen.Morphology
( makeLexicalInflection
, cleanInflectionSys
) where

import Prelude hiding (Word)
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Control.Monad

import Data.Phoneme
import Data.Inflection
import Data.Grammar

import Gen.Inflection
import Gen.Root

-- Inflections for each lexical category
makeLexicalInflection :: [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> InflectionMap -> (LexCat, Int, Int, Int) -> RVar [ManifestSystem]
makeLexicalInflection vows ccs inflMap (lc, i, j, k) = do
  part <- makeParticleSystems lc i vows ccs inflMap
  pref <- makePrefixSystems lc j vows ccs inflMap
  suff <- makeSuffixSystems lc k vows ccs inflMap
  return (part ++ pref ++ suff)

-- Prefixs
makePrefixSystems :: LexCat -> Int -> [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> InflectionMap -> RVar [ManifestSystem]
makePrefixSystems lc 0 vows ccs gramSys = return []
makePrefixSystems lc i vows ccs gramSys = (++) <$> makePrefixSystems lc (i-1) vows ccs gramSys <*> ((:[]) <$> makePrefixSystem lc i vows ccs gramSys)

makePrefixSystem :: LexCat -> Int -> [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> InflectionMap -> RVar ManifestSystem
makePrefixSystem lc i vows ccs gramSys = do
  let (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanInflectionSys gramSys lc Prefix i
  let combos = (,,,,,,,,,,,,,,,,) <$> gen <*> ani <*> cas <*> num <*> def <*> spe <*> top <*> per <*> hon <*> pol <*> ten <*> asp <*> moo <*> voi <*> evi <*> tra <*> vol
  morphs <- replicateM (length combos) (makeRoot vows ccs (1, 1))
  return $ ManifestSystem lc Prefix (zip morphs combos)

-- Suffixs
makeSuffixSystems :: LexCat -> Int -> [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> InflectionMap -> RVar [ManifestSystem]
makeSuffixSystems lc 0 vows scs gramSys = return []
makeSuffixSystems lc i vows ccs gramSys = (++) <$> makeSuffixSystems lc (i-1) vows ccs gramSys <*> ((:[]) <$> makeSuffixSystem lc i vows ccs gramSys)

makeSuffixSystem :: LexCat -> Int -> [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> InflectionMap -> RVar ManifestSystem
makeSuffixSystem lc i vows ccs gramSys = do
  let (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanInflectionSys gramSys lc Suffix i
  let combos = (,,,,,,,,,,,,,,,,) <$> gen <*> ani <*> cas <*> num <*> def <*> spe <*> top <*> per <*> hon <*> pol <*> ten <*> asp <*> moo <*> voi <*> evi <*> tra <*> vol
  morphs <- replicateM (length combos) (makeRoot vows ccs (1, 1))
  return $ ManifestSystem lc Suffix (zip morphs combos)

-- Particles
makeParticleSystems :: LexCat -> Int -> [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> InflectionMap -> RVar [ManifestSystem]
makeParticleSystems lc 0 vows ccs gramSys = return []
makeParticleSystems lc i vows ccs gramSys = (++) <$> makeParticleSystems lc (i-1) vows ccs gramSys <*> ((:[]) <$> makeParticleSystem lc i vows ccs gramSys)

makeParticleSystem :: LexCat -> Int -> [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> InflectionMap -> RVar ManifestSystem
makeParticleSystem lc i vows ccs gramSys = do
  let (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanInflectionSys gramSys lc Particle i
  let combos = (,,,,,,,,,,,,,,,,) <$> gen <*> ani <*> cas <*> num <*> def <*> spe <*> top <*> per <*> hon <*> pol <*> ten <*> asp <*> moo <*> voi <*> evi <*> tra <*> vol
  morphs <- replicateM (length combos) (makeRoot vows ccs (1, 2))
  return $ ManifestSystem lc Particle (zip morphs combos)

-- Clean sys
cleanSys :: Manifest [a] -> LexCat -> ManifestType -> Int -> [Express a]
cleanSys NoManifest _ _ _ = [NoExpress]
cleanSys (Manifest t x) lc mt i = out where
  filt = filter (== (lc, mt, i)) t
  out
    | null filt = [NoExpress]
    | otherwise = map Express x

cleanInflectionSys :: InflectionMap -> LexCat -> ManifestType -> Int -> ([Express Gender], [Express Animacy], [Express Case], [Express Number], [Express Definiteness], [Express Specificity], [Express Topic], [Express Person], [Express Honorific], [Express Polarity], [Express Tense], [Express Aspect], [Express Mood], [Express Voice], [Express Evidentiality], [Express Transitivity], [Express Volition])
cleanInflectionSys inflMap lc mt i = (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) where
  gen = cleanSys (genSys inflMap) lc mt i
  ani = cleanSys (aniSys inflMap) lc mt i
  cas = cleanSys (casSys inflMap) lc mt i
  num = cleanSys (numSys inflMap) lc mt i
  def = cleanSys (defSys inflMap) lc mt i
  spe = cleanSys (speSys inflMap) lc mt i
  top = cleanSys (topSys inflMap) lc mt i
  per = cleanSys (perSys inflMap) lc mt i
  hon = cleanSys (honSys inflMap) lc mt i
  pol = cleanSys (polSys inflMap) lc mt i
  ten = cleanSys (tenSys inflMap) lc mt i
  asp = cleanSys (aspSys inflMap) lc mt i
  moo = cleanSys (mooSys inflMap) lc mt i
  voi = cleanSys (voiSys inflMap) lc mt i
  evi = cleanSys (eviSys inflMap) lc mt i
  tra = cleanSys (traSys inflMap) lc mt i
  vol = cleanSys (volSys inflMap) lc mt i
