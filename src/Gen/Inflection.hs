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
  (genSys, genNs) <- decideDeclension (inputGender idata) []
  (aniSys, aniNs) <- decideDeclension (inputAnimacy idata) genNs
  (casSys, casNs) <- decideDeclension (inputCase idata) aniNs
  (numSys, numNs) <- decideDeclension (inputNumber idata) casNs
  (defSys, defNs) <- decideDeclension (inputDefiniteness idata) numNs
  (speSys, speNs) <- decideDeclension (inputSpecificity idata) defNs
  (topSys, topNs) <- decideDeclension (inputTopic idata) speNs
  (perSys, perNs) <- decideDeclension (inputPerson idata) topNs
  (honSys, honNs) <- decideBoth (inputHonorific idata) perNs
  (polSys, polNs) <- decideBoth (inputPolarity idata) honNs
  (tenSys, tenNs) <- decideConjugation (inputTense idata) polNs
  (aspSys, aspNs) <- decideConjugation (inputAspect idata) tenNs
  (mooSys, mooNs) <- decideConjugation (inputMood idata) aspNs
  (voiSys, voiNs) <- decideConjugation (inputVoice idata) mooNs
  (eviSys, eviNs) <- decideConjugation (inputEvidentiality idata) voiNs
  (traSys, traNs) <- decideConjugation (inputTransitivity idata) eviNs
  (volSys, volNs) <- decideConjugation (inputVolition idata) traNs
  return (InflectionMap genSys aniSys casSys numSys defSys speSys topSys perSys honSys polSys tenSys aspSys mooSys voiSys eviSys traSys volSys, volNs)

-- Each new GramCat can either be expressed in a new Part/Pref/Suff or an existing one
-- There can only be one Transfix per LexCat
-- These two functions basically decide that
-- This decides the degree of fusional/agglutinativeness
bar2 :: [(LexCat, Int, Int, Int, Int, Int)] -> [LexCat] -> Maybe LexCat -> RVar ([ManifestPlace], [(LexCat, Int, Int, Int, Int, Int)])
bar2 ks [] _ = return ([],ks)
bar2 ks lcs agr = do
  (mp, ks_) <- rab2 ks (unsafeHead lcs) agr
  first ((:) mp) <$> bar2 ks_ (unsafeTail lcs) agr

rab2 :: [(LexCat, Int, Int, Int, Int, Int)] -> LexCat -> Maybe LexCat -> RVar (ManifestPlace, [(LexCat, Int, Int, Int, Int, Int)])
rab2 counts lc agr = join out where
    (lcCounts, restCounts) = partition (\(c, _, _, _, _, _) -> c == lc) counts
    lcCounts_
      | null lcCounts = (lc, 0, 0, 0, 0, 0)
      | otherwise = unsafeHead lcCounts
    (_, part, pref, suff, trans, ctrans) = lcCounts_

    -- decide between Particle, Prefix, Suffix, Transfix
    out = choice ( [ do
                     i <- uniform 1 (part+1) -- decide between an existing one or a new one
                     return (ManifestPlace lc [(Particle, i)] agr, (lc, max part i, pref, suff, trans, ctrans) : restCounts)
                   , do
                     j <- uniform 1 (pref+1)
                     return (ManifestPlace lc [(Prefix, j)] agr, (lc, part, max pref j, suff, trans, ctrans) : restCounts)
                   , do
                     k <- uniform 1 (suff+1)
                     return (ManifestPlace lc [(Suffix, k)] agr, (lc, part, pref, max suff k, trans, ctrans) : restCounts)
                   ] ++ transOut ++ ctransOut
                 )
    transOut = if ctrans > 0 then [] else [ return (ManifestPlace lc [(Transfix, 1)] agr, (lc, part, pref, suff, 1, ctrans) : restCounts) ]
    ctransOut = if trans > 0 then [] else [ return (ManifestPlace lc [(CTransfix, 1)] agr, (lc, part, pref, suff, trans, 1) : restCounts) ]


decideDeclension :: [[a]] -> [(LexCat, Int, Int, Int, Int, Int)] -> RVar (Manifest a, [(LexCat, Int, Int, Int, Int, Int)])
decideDeclension catdata counts = do
  gcatsys <- choice catdata
  (mpNoun, countsN) <- rab2 counts Noun Nothing
  agrCats <- randomSubset [Verb, Adj, Det]
  (mpsAgr, countsN_) <- bar2 countsN agrCats (Just Noun)
  choice [(NoManifest, counts), (Manifest (mpNoun:mpsAgr) gcatsys, countsN_)]

decideConjugation :: [[a]] -> [(LexCat, Int, Int, Int, Int, Int)] -> RVar (Manifest a, [(LexCat, Int, Int, Int, Int, Int)])
decideConjugation catdata counts = do
  catSys <- choice catdata
  (mpVerb, countsN) <- rab2 counts Verb Nothing
  agrCats <- randomSubset [Adv]
  (mpsAgr, countsN_) <- bar2 countsN agrCats (Just Verb)
  choice [(NoManifest, counts), (Manifest (mpVerb:mpsAgr) catSys, countsN_)]

decideBoth:: [[a]] -> [(LexCat, Int, Int, Int, Int, Int)] -> RVar (Manifest a, [(LexCat, Int, Int, Int, Int, Int)])
decideBoth catdata counts = do
  catSys <- choice catdata
  (cats, agrCatsN, agrCatsV) <- choice [([Verb, Noun], [Adj, Det], [Adv]), ([Verb],[],[Adv]), ([Noun],[Verb, Adj, Det],[])]
  (mps, countsN) <- bar2 counts cats Nothing
  agrCats <- randomSubset agrCatsN
  agrCats_ <- randomSubset agrCatsV
  (mpsAgr, countsN_) <- bar2 countsN agrCats (Just Noun)
  (mpsAgr_, countsN__) <- bar2 countsN_ agrCats_ (Just Verb)
  -- decide if <gram category> even manifests itself at all
  choice [(NoManifest, counts), (Manifest (mps ++ mpsAgr ++ mpsAgr_) catSys, countsN__)]
