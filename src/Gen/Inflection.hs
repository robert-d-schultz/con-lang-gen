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
-- Example: [(Noun, 2, 0, 0, 0, 0), (Verb, 0, 2, 0, 0, 0)] means Nouns have two particles, Verbs have two prefixes
makeInflectionMap :: InputData -> RVar (InflectionMap, [(LexCat, Int, Int, Int, Int, Int)])
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
-- There can only be one Transfix per LexCat
-- These two functions basically decide that
-- This decides the degree of fusional/agglutinativeness
bar2 :: [(LexCat, Int, Int, Int, Int, Int)] -> [(LexCat, MorphType, Int)] -> [LexCat] -> RVar ([(LexCat, MorphType, Int)], [(LexCat, Int, Int, Int, Int, Int)])
bar2 ks ts [] = return (ts,ks)
bar2 ks ts lcs = do
  (newt, newks) <- rab2 ks (unsafeHead lcs)
  bar2 newks (newt : ts) (unsafeTail lcs)

rab2 :: [(LexCat, Int, Int, Int, Int, Int)] -> LexCat -> RVar ((LexCat, MorphType, Int), [(LexCat, Int, Int, Int, Int, Int)])
rab2 lcs lc = join out where
    (fu, ba) = partition (\(c, _, _, _, _, _) -> c == lc) lcs
    shit
      | null fu = (lc, 0, 0, 0, 0, 0)
      | otherwise = unsafeHead fu
    (_, part, pref, suff, trans, ctrans) = shit

    -- decide between Particle, Prefix, Suffix, Transfix
    out = choice ( [ do
                     i <- uniform 1 (part+1) -- decide between an existing one or a new one
                     return ((lc, Particle, i), (lc, max i part, pref, suff, trans, ctrans) : ba)
                   , do
                     j <- uniform 1 (pref+1)
                     return ((lc, Prefix, j), (lc, part, max j pref, suff, trans, ctrans) : ba)
                   , do
                     k <- uniform 1 (suff+1)
                     return ((lc, Suffix, k), (lc, part, pref, max k suff, trans, ctrans) : ba)
                   ] ++ transOut ++ ctransOut
                 )
    transOut = if ctrans > 0 then [] else [ return ((lc, Transfix, 1), (lc, part, pref, suff, 1, ctrans) : ba) ]
    ctransOut = if trans > 0 then [] else [ return ((lc, CTransfix, 1), (lc, part, pref, suff, trans, 1) : ba) ]


fooDeclension :: [[a]] -> [(LexCat, Int, Int, Int, Int, Int)] -> RVar (Manifest a, [(LexCat, Int, Int, Int, Int, Int)])
fooDeclension catdata prevNs = do
  -- pick the <gram category> system...
  gcatsys <- choice catdata
  -- pick which lexcats <gram category> manifests on (related to agreement)
  cats <- (:) Noun <$> randomSubset [Adj, Verb]
  -- bar function... no idea what it does
  (ts, ns) <- bar2 prevNs [] cats
  -- decide if <gram category> even manifests itself at all
  choice [(NoManifest, prevNs), (Manifest ts gcatsys, ns)]

fooConjugation :: [[a]] -> [(LexCat, Int, Int, Int, Int, Int)] -> RVar (Manifest a, [(LexCat, Int, Int, Int, Int, Int)])
fooConjugation catdata prevNs = do
  gcatsys <- choice catdata
  cats <- (:) Verb . concat <$> randomSubset [[Adv], [Noun, Pron]]
  (ts, ns) <- bar2 prevNs [] cats
  choice [(NoManifest, prevNs), (Manifest ts gcatsys, ns)]

fooBoth :: [[a]] -> [(LexCat, Int, Int, Int, Int, Int)] -> RVar (Manifest a, [(LexCat, Int, Int, Int, Int, Int)])
fooBoth catdata prevNs = do
  gcatsys <- choice catdata
  cats1 <- (++) [Noun, Pron] <$> randomSubset [Adj, Verb]
  cats2 <- (:) Verb . concat <$> randomSubset [[Adv], [Noun, Pron]]
  cats <- choice [cats1, cats2, ordNub $ cats1 ++ cats2]
  (ts, ns) <- bar2 prevNs [] cats
  choice [(NoManifest, prevNs), (Manifest ts gcatsys, ns)]
