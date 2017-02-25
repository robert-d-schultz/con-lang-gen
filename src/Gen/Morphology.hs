module Gen.Morphology
( makeLexicalInflection
, cleanGrammarSys
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
makeLexicalInflection :: [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> InflectionSystem -> (LexCat, Int, Int, Int) -> RVar (LexCat, [ManifestSystem], [ManifestSystem], [ManifestSystem])
makeLexicalInflection vows ccs inflSys (lc, part, pref, suff) = (,,,) lc <$> makeParticleSystems lc part vows ccs inflSys <*> makePrefixSystems lc pref vows ccs inflSys <*> makeSuffixSystems lc suff vows ccs inflSys

-- Prefixs
makePrefixSystems :: LexCat -> Int -> [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> InflectionSystem -> RVar [ManifestSystem]
makePrefixSystems lc 0 vows ccs gramSys = return []
makePrefixSystems lc i vows ccs gramSys = (++) <$> makePrefixSystems lc (i-1) vows ccs gramSys <*> ((:[]) <$> makePrefixSystem lc i vows ccs gramSys)

makePrefixSystem :: LexCat -> Int -> [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> InflectionSystem -> RVar ManifestSystem
makePrefixSystem lc i vows ccs gramSys = do
  let (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanGrammarSys gramSys lc Prefix i
  let combos = (,,,,,,,,,,,,,,,,) <$> gen <*> ani <*> cas <*> num <*> def <*> spe <*> top <*> per <*> hon <*> pol <*> ten <*> asp <*> moo <*> voi <*> evi <*> tra <*> vol
  morphs <- replicateM (length combos) (makeRoot vows ccs (1, 4))
  return $ ManifestSystem lc Prefix (zip morphs combos)

-- Suffixs
makeSuffixSystems :: LexCat -> Int -> [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> InflectionSystem -> RVar [ManifestSystem]
makeSuffixSystems lc 0 vows scs gramSys = return []
makeSuffixSystems lc i vows ccs gramSys = (++) <$> makeSuffixSystems lc (i-1) vows ccs gramSys <*> ((:[]) <$> makeSuffixSystem lc i vows ccs gramSys)

makeSuffixSystem :: LexCat -> Int -> [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> InflectionSystem -> RVar ManifestSystem
makeSuffixSystem lc i vows ccs gramSys = do
  let (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanGrammarSys gramSys lc Suffix i
  let combos = (,,,,,,,,,,,,,,,,) <$> gen <*> ani <*> cas <*> num <*> def <*> spe <*> top <*> per <*> hon <*> pol <*> ten <*> asp <*> moo <*> voi <*> evi <*> tra <*> vol
  morphs <- replicateM (length combos) (makeRoot vows ccs (1, 2))
  return $ ManifestSystem lc Suffix (zip morphs combos)

-- Particles
makeParticleSystems :: LexCat -> Int -> [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> InflectionSystem -> RVar [ManifestSystem]
makeParticleSystems lc 0 vows ccs gramSys = return []
makeParticleSystems lc i vows ccs gramSys = (++) <$> makeParticleSystems lc (i-1) vows ccs gramSys <*> ((:[]) <$> makeParticleSystem lc i vows ccs gramSys)

makeParticleSystem :: LexCat -> Int -> [Phoneme] -> ([[Phoneme]], [[Phoneme]]) -> InflectionSystem -> RVar ManifestSystem
makeParticleSystem lc i vows ccs gramSys = do
  let (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanGrammarSys gramSys lc Particle i
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

cleanGrammarSys :: InflectionSystem -> LexCat -> ManifestType -> Int -> ([Express Gender], [Express Animacy], [Express Case], [Express Number], [Express Definiteness], [Express Specificity], [Express Topic], [Express Person], [Express Honorific], [Express Polarity], [Express Tense], [Express Aspect], [Express Mood], [Express Voice], [Express Evidentiality], [Express Transitivity], [Express Volition])
cleanGrammarSys gramSys lc mt i = (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) where
  gen = cleanSys (genSys gramSys) lc mt i
  ani = cleanSys (aniSys gramSys) lc mt i
  cas = cleanSys (casSys gramSys) lc mt i
  num = cleanSys (numSys gramSys) lc mt i
  def = cleanSys (defSys gramSys) lc mt i
  spe = cleanSys (speSys gramSys) lc mt i
  top = cleanSys (topSys gramSys) lc mt i
  per = cleanSys (perSys gramSys) lc mt i
  hon = cleanSys (honSys gramSys) lc mt i
  pol = cleanSys (polSys gramSys) lc mt i
  ten = cleanSys (tenSys gramSys) lc mt i
  asp = cleanSys (aspSys gramSys) lc mt i
  moo = cleanSys (mooSys gramSys) lc mt i
  voi = cleanSys (voiSys gramSys) lc mt i
  evi = cleanSys (eviSys gramSys) lc mt i
  tra = cleanSys (traSys gramSys) lc mt i
  vol = cleanSys (volSys gramSys) lc mt i
