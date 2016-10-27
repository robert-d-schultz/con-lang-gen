module MorphologyGen
( makeLexicalInflection
, cleanGrammarSys
) where

import Prelude hiding (Word)
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Control.Monad

import PhonemeData
import InflectionGen
import InflectionData
import WordGen

-- Inflections for each lexical category
makeLexicalInflection :: [Phoneme] -> [[Phoneme]] -> InflectionSystem -> (LexicalCategory, Int, Int, Int, Int) -> RVar (LexicalCategory, [ManifestSystem], [ManifestSystem], [ManifestSystem], [ManifestSystem])
makeLexicalInflection vows sonHier inflSys (lc, prep, posp, pref, suff) = (,,,,) lc <$> makePreParticleSystems lc prep vows sonHier inflSys <*> makePostParticleSystems lc posp vows sonHier inflSys <*> makePrefixSystems lc pref vows sonHier inflSys <*> makeSuffixSystems lc suff vows sonHier inflSys

-- Prefixes
makePrefixSystems :: LexicalCategory -> Int -> [Phoneme] -> [[Phoneme]] -> InflectionSystem -> RVar [ManifestSystem]
makePrefixSystems lc 0 vows sonHier gramSys = return []
makePrefixSystems lc i vows sonHier gramSys = (++) <$> makePrefixSystems lc (i-1) vows sonHier gramSys <*> ((:[]) <$> makePrefixSystem lc i vows sonHier gramSys)

makePrefixSystem :: LexicalCategory -> Int -> [Phoneme] -> [[Phoneme]] -> InflectionSystem -> RVar ManifestSystem
makePrefixSystem lc i vows sonHier gramSys = do
  let (gen,ani,cas,num,def,spe,top,per,clu,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanGrammarSys gramSys lc Prefix i
  let combos = (,,,,,,,,,,,,,,,,,) <$> gen <*> ani <*> cas <*> num <*> def <*> spe <*> top <*> per <*> clu <*> hon <*> pol <*> ten <*> asp <*> moo <*> voi <*> evi <*> tra <*> vol
  morphs <- replicateM (length combos) (makeMorpheme vows sonHier ((0, 1), (1, 2), (0, 0), (0, 0)))
  return $ ManifestSystem lc Prefix (zip morphs combos)

-- Suffixes
makeSuffixSystems :: LexicalCategory -> Int -> [Phoneme] -> [[Phoneme]] -> InflectionSystem -> RVar [ManifestSystem]
makeSuffixSystems lc 0 vows sonHier gramSys = return []
makeSuffixSystems lc i vows sonHier gramSys = (++) <$> makeSuffixSystems lc (i-1) vows sonHier gramSys <*> ((:[]) <$> makeSuffixSystem lc i vows sonHier gramSys)

makeSuffixSystem :: LexicalCategory -> Int -> [Phoneme] -> [[Phoneme]] -> InflectionSystem -> RVar ManifestSystem
makeSuffixSystem lc i vows sonHier gramSys = do
  let (gen,ani,cas,num,def,spe,top,per,clu,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanGrammarSys gramSys lc Suffix i
  let combos = (,,,,,,,,,,,,,,,,,) <$> gen <*> ani <*> cas <*> num <*> def <*> spe <*> top <*> per <*> clu <*> hon <*> pol <*> ten <*> asp <*> moo <*> voi <*> evi <*> tra <*> vol
  morphs <- replicateM (length combos) (makeMorpheme vows sonHier ((0, 1), (0, 0), (0, 0), (1, 2)))
  return $ ManifestSystem lc Suffix (zip morphs combos)

-- Pre-position particles
makePreParticleSystems :: LexicalCategory -> Int -> [Phoneme] -> [[Phoneme]] -> InflectionSystem -> RVar [ManifestSystem]
makePreParticleSystems lc 0 vows sonHier gramSys = return []
makePreParticleSystems lc i vows sonHier gramSys = (++) <$> makePreParticleSystems lc (i-1) vows sonHier gramSys <*> ((:[]) <$> makePreParticleSystem lc i vows sonHier gramSys)

makePreParticleSystem :: LexicalCategory -> Int -> [Phoneme] -> [[Phoneme]] -> InflectionSystem -> RVar ManifestSystem
makePreParticleSystem lc i vows sonHier gramSys = do
  let (gen,ani,cas,num,def,spe,top,per,clu,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanGrammarSys gramSys lc PreParticle i
  let combos = (,,,,,,,,,,,,,,,,,) <$> gen <*> ani <*> cas <*> num <*> def <*> spe <*> top <*> per <*> clu <*> hon <*> pol <*> ten <*> asp <*> moo <*> voi <*> evi <*> tra <*> vol
  morphs <- replicateM (length combos) (makeMorpheme vows sonHier ((1, 2), (0, 2), (1, 2), (0, 2)))
  return $ ManifestSystem lc PreParticle (zip morphs combos)

-- Post-position particles
makePostParticleSystems :: LexicalCategory -> Int -> [Phoneme] -> [[Phoneme]] -> InflectionSystem -> RVar [ManifestSystem]
makePostParticleSystems lc 0 vows sonHier gramSys = return []
makePostParticleSystems lc i vows sonHier gramSys = (++) <$> makePostParticleSystems lc (i-1) vows sonHier gramSys <*> ((:[]) <$> makePostParticleSystem lc i vows sonHier gramSys)

makePostParticleSystem :: LexicalCategory -> Int -> [Phoneme] -> [[Phoneme]] -> InflectionSystem -> RVar ManifestSystem
makePostParticleSystem lc i vows sonHier gramSys = do
  let (gen,ani,cas,num,def,spe,top,per,clu,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanGrammarSys gramSys lc PostParticle i
  let combos = (,,,,,,,,,,,,,,,,,) <$> gen <*> ani <*> cas <*> num <*> def <*> spe <*> top <*> per <*> clu <*> hon <*> pol <*> ten <*> asp <*> moo <*> voi <*> evi <*> tra <*> vol
  morphs <- replicateM (length combos) (makeMorpheme vows sonHier ((1, 2), (0, 2), (1, 2), (0, 2)))
  return $ ManifestSystem lc PostParticle (zip morphs combos)

-- Clean sys
cleanSys :: Manifest [a] -> LexicalCategory -> ManifestType -> Int -> [Manifest a]
cleanSys NoManifest _ _ _ = [NoManifest]
cleanSys (Manifest t x) lc mt i = out where
  filt = filter (== (lc, mt, i)) t
  out
    | null filt = [NoManifest]
    | otherwise = map (Manifest t) x

cleanGrammarSys :: InflectionSystem -> LexicalCategory -> ManifestType -> Int -> ([Manifest Gender], [Manifest Animacy], [Manifest Case], [Manifest Number], [Manifest Definiteness], [Manifest Specificity], [Manifest Topic], [Manifest Person], [Manifest Clusivity], [Manifest Honorific], [Manifest Polarity], [Manifest Tense], [Manifest Aspect], [Manifest Mood], [Manifest Voice], [Manifest Evidentiality], [Manifest Transitivity], [Manifest Volition])
cleanGrammarSys gramSys lc mt i = (gen,ani,cas,num,def,spe,top,per,clu,hon,pol,ten,asp,moo,voi,evi,tra,vol) where
  gen = cleanSys (genSys gramSys) lc mt i
  ani = cleanSys (aniSys gramSys) lc mt i
  cas = cleanSys (casSys gramSys) lc mt i
  num = cleanSys (numSys gramSys) lc mt i
  def = cleanSys (defSys gramSys) lc mt i
  spe = cleanSys (speSys gramSys) lc mt i
  top = cleanSys (topSys gramSys) lc mt i
  per = cleanSys (perSys gramSys) lc mt i
  clu = cleanSys (cluSys gramSys) lc mt i
  hon = cleanSys (honSys gramSys) lc mt i
  pol = cleanSys (polSys gramSys) lc mt i
  ten = cleanSys (tenSys gramSys) lc mt i
  asp = cleanSys (aspSys gramSys) lc mt i
  moo = cleanSys (mooSys gramSys) lc mt i
  voi = cleanSys (voiSys gramSys) lc mt i
  evi = cleanSys (eviSys gramSys) lc mt i
  tra = cleanSys (traSys gramSys) lc mt i
  vol = cleanSys (volSys gramSys) lc mt i
