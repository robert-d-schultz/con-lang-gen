module MorphologyGen
( makeExponentSystems
, makeParticleSystems
, cleanGrammarSysE
) where

import Prelude hiding (Word)
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Control.Monad

import PhonemeType2
import GrammarGen2
import GrammarType2
import WordGen2


makeExponentSystems :: Int -> [Phoneme] -> [[Phoneme]] -> GrammarSystem -> RVar [ExponentSystem]
makeExponentSystems 0 vows sonHier gramSys = return []
makeExponentSystems i vows sonHier gramSys = (++) <$> makeExponentSystems (i-1) vows sonHier gramSys <*> ((:[]) <$> makeExponentSystem i vows sonHier gramSys)

makeExponentSystem :: Int -> [Phoneme] -> [[Phoneme]] -> GrammarSystem -> RVar ExponentSystem
makeExponentSystem i vows sonHier gramSys = do
  let (g, a, c, n, h, d, s) = cleanGrammarSysE i gramSys
  let combos = (,,,,,,) <$> g <*> a <*> c <*> n <*> h <*> d <*> s
  morphs <- replicateM (length combos) (makeMorpheme vows sonHier ((0, 1), (0, 0), (0, 0), (1, 3)))
  return $ ExponentSystem (zip morphs combos)

makeParticleSystems :: Int -> [Phoneme] -> [[Phoneme]] -> GrammarSystem -> RVar [ParticleSystem]
makeParticleSystems 0 vows sonHier gramSys = return []
makeParticleSystems i vows sonHier gramSys = (++) <$> makeParticleSystems (i-1) vows sonHier gramSys <*> ((:[]) <$> makeParticleSystem i vows sonHier gramSys)

makeParticleSystem :: Int -> [Phoneme] -> [[Phoneme]] -> GrammarSystem -> RVar ParticleSystem
makeParticleSystem i vows sonHier gramSys = do
  let (g, a, c, n, h, d, s) = cleanGrammarSysP i gramSys
  let combos = (,,,,,,) <$> g <*> a <*> c <*> n <*> h <*> d <*> s
  morphs <- replicateM (length combos) (makeMorpheme vows sonHier ((0, 1), (1, 2), (0, 1), (1, 2)))
  return $ ParticleSystem (zip morphs combos)

cleanSysE :: Int -> Manifest ([a], Int) -> [Manifest a]
cleanSysE i (Exponent (x, j))
  | i == j = map Exponent x
  | otherwise = [NoManifest]
cleanSysE i _ = [NoManifest]

cleanGrammarSysE :: Int -> GrammarSystem -> ([Manifest Gender], [Manifest Animacy], [Manifest Case], [Manifest Number], [Manifest Honorific], [Manifest Definiteness], [Manifest Specificity])
cleanGrammarSysE i gramSys = (g, a, c, n, h, d, s) where
  g = cleanSysE i (gSys gramSys)
  a = cleanSysE i (aSys gramSys)
  c = cleanSysE i (cSys gramSys)
  n = cleanSysE i (nSys gramSys)
  h = cleanSysE i (hSys gramSys)
  d = cleanSysE i (dSys gramSys)
  s = cleanSysE i (sSys gramSys)

cleanSysP :: Int -> Manifest ([a], Int) -> [Manifest a]
cleanSysP i (Particle (x, j))
  | i == j = map Particle x
  | otherwise = [NoManifest]
cleanSysP i _ = [NoManifest]

cleanGrammarSysP :: Int -> GrammarSystem -> ([Manifest Gender], [Manifest Animacy], [Manifest Case], [Manifest Number], [Manifest Honorific], [Manifest Definiteness], [Manifest Specificity])
cleanGrammarSysP i gramSys = (g, a, c, n, h, d, s) where
  g = cleanSysP i (gSys gramSys)
  a = cleanSysP i (aSys gramSys)
  c = cleanSysP i (cSys gramSys)
  n = cleanSysP i (nSys gramSys)
  h = cleanSysP i (hSys gramSys)
  d = cleanSysP i (dSys gramSys)
  s = cleanSysP i (sSys gramSys)
