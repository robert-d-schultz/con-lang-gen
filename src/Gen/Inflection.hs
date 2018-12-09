module Gen.Inflection
( makeInflectionMap
) where

import ClassyPrelude hiding (Word, union)

import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)

import Data.Inflection

import LoadStuff

import HelperFunctions

-- Outputs an inflection map as well as the number of Particles,Prefixes,Suffixes there are for each LexCat
-- Example: [(Noun, 2, 0, 0), (Verb, 0, 2, 0)] means Nouns have two particles, Verbs have two prefixes
makeInflectionMap :: InputData -> RVar (InflectionMap, [(LexCat, Int, Int, Int)])
makeInflectionMap idata = do
  (genSys, genNs) <- fooDeclension (inputGender idata) []
  (aniSys, aniNs) <- fooDeclension (inputAnimacy idata) genNs
  (casSys, casNs) <- fooDeclension (inputCase idata) aniNs
  (numSys, numNs) <- fooDeclension (inputNumber idata) casNs
  (defSys, defNs) <- fooDeclension (inputDefiniteness idata) numNs
  (speSys, speNs) <- fooDeclension (inputSpecificity idata) defNs
  (topSys, topNs) <- fooDeclension (inputTopic idata) speNs
  (perSys, perNs) <- fooDeclension (inputPerson idata) topNs
  (honSys, honNs) <- fooBoth (inputHonorific idata) perNs
  (polSys, polNs) <- fooBoth (inputPolarity idata) honNs
  (tenSys, tenNs) <- fooConjugation (inputTense idata) polNs
  (aspSys, aspNs) <- fooConjugation (inputAspect idata) tenNs
  (mooSys, mooNs) <- fooConjugation (inputMood idata) aspNs
  (voiSys, voiNs) <- fooConjugation (inputVoice idata) mooNs
  (eviSys, eviNs) <- fooConjugation (inputEvidentiality idata) voiNs
  (traSys, traNs) <- fooConjugation (inputTransitivity idata) eviNs
  (volSys, volNs) <- fooConjugation (inputVolition idata) traNs
  return (InflectionMap genSys aniSys casSys numSys defSys speSys topSys perSys honSys polSys tenSys aspSys mooSys voiSys eviSys traSys volSys, volNs)

-- Each new GramCat can either be expressed in a new Part/Pref/Suff or an existing one
-- These two functions basically decide that
-- This decides the degree of fusional/agglutinativeness
bar :: [(LexCat, Int, Int, Int)] -> [(LexCat, InflType, Int)] -> [LexCat] -> RVar ([(LexCat, InflType, Int)], [(LexCat, Int, Int, Int)])
bar ks ts [] = return (ts,ks)
bar ks ts lcs = do
  (newt, newks) <- rab ks (unsafeHead lcs)
  bar newks (newt : ts) (unsafeTail lcs)

rab :: [(LexCat, Int, Int, Int)] -> LexCat -> RVar ((LexCat, InflType, Int), [(LexCat, Int, Int, Int)])
rab lcs lc2 = join out where
    (fu, ba) = partition (\(c, _, _, _) -> c == lc2) lcs
    shit
      | null fu = (lc2, 0, 0, 0)
      | otherwise = unsafeHead fu
    (lc, part, pref, suff) = shit

    -- decide between Particle, Prefix, Suffix
    out = choice [ do
                   i <- uniform 1 (part+1) -- decide between an existing one or a new one
                   return ((lc, Particle, i), (lc, max i part, pref, suff) : ba)
                 , do
                   j <- uniform 1 (pref+1)
                   return ((lc, Prefix, j), (lc, part, max j pref, suff) : ba)
                 , do
                   j <- uniform 1 (suff+1)
                   return ((lc, Suffix, j), (lc, part, pref, max j suff) : ba)
                 ]


fooDeclension :: [[a]] -> [(LexCat, Int, Int, Int)] -> RVar (Manifest a, [(LexCat, Int, Int, Int)])
fooDeclension catdata prevNs = do
  -- pick the <gram category> system...
  gcatsys <- choice catdata
  -- pick which lexcats <gram category> manifests on (related to agreement)
  cats <- (:) Noun <$> randomSubset [Adj, Verb]
  -- bar function... no idea what it does
  (ts, ns) <- bar prevNs [] cats
  -- decide if <gram category> even manifests itself at all
  choice [(NoManifest, prevNs), (Manifest ts gcatsys, ns)]

fooConjugation :: [[a]] -> [(LexCat, Int, Int, Int)] -> RVar (Manifest a, [(LexCat, Int, Int, Int)])
fooConjugation catdata prevNs = do
  gcatsys <- choice catdata
  cats <- (:) Verb <$> randomSubset [Adv, Noun]
  (ts, ns) <- bar prevNs [] cats
  choice [(NoManifest, prevNs), (Manifest ts gcatsys, ns)]

fooBoth :: [[a]] -> [(LexCat, Int, Int, Int)] -> RVar (Manifest a, [(LexCat, Int, Int, Int)])
fooBoth catdata prevNs = do
  gcatsys <- choice catdata
  cats1 <- (:) Noun <$> randomSubset [Adj, Verb]
  cats2 <- (:) Verb <$> randomSubset [Adv, Noun]
  cats <- choice [cats1, cats2, ordNub $ cats1 ++ cats2]
  (ts, ns) <- bar prevNs [] cats
  choice [(NoManifest, prevNs), (Manifest ts gcatsys, ns)]
