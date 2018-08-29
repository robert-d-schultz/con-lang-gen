module Morph.Phonology
( phonologicalChange
) where

import ClassyPrelude hiding ((\\))

import Data.RVar
import Data.Random.Extras
import Data.List (nub, (\\))

import Data.Language
import Data.Phoneme
import Data.Inflection
import Data.Soundchange

import Constants
import HelperFunctions

-- Given a phonological rule, change some phonemes into previously-not-existing ones
phonologicalChange :: Int -> Language -> RVar Language
phonologicalChange 50 lang = trace "Gave up on sound change" return lang --give up after 50 attempts
phonologicalChange i lang = do
  rule <- generateRule lang
  let (langN, changes) = executeRuleOnLanguage rule lang
  if changes < 2 then phonologicalChange (i+1) lang else return langN

executeRuleOnLanguage :: Rule -> Language -> (Language, Int)
executeRuleOnLanguage rule lang = (langN, rootChanges + inflChanges) where

  -- Execute rule on each (valid) phoneme in the lexicon and inflection system
  roots = getRoots lang
  manSyss = getManSyss lang

  (rootsN, rootChanges) = second sum $ unzip $ map (\(x, SyllWord y) -> first ((,) x . SyllWord) (executeRuleOnSyllWord 0 rule [] y)) roots :: ([((Text, LexCat), SyllWord)], Int)
  (manSyssN, inflChanges) = second sum $ unzip $ map (\(ManifestSystem x y z) -> first (ManifestSystem x y) (second sum $ unzip $ map (\(SyllWord w, v) -> first (swap . (,) v . SyllWord) (executeRuleOnSyllWord 0 rule [] w)) z)) manSyss :: ([ManifestSystem], Int)

  -- Take a new survey of which phonemes exist in the lexicon
  -- Update inventories and maps
  lexPhonemes = concatMap syllToPhonemes (concatMap (\(_, SyllWord y) -> y) rootsN)
  declPhonemes = concatMap syllToPhonemes (concatMap (\(ManifestSystem _ _ z) -> (concatMap (\(SyllWord w, _) -> w) z)) manSyssN)
  allPhonemes = nub $ lexPhonemes ++ declPhonemes
  cInvN = filter isConsonant allPhonemes
  vInvN = filter isVowel allPhonemes
  cMapN = updateCMap cInvN
  vMapN = updateVMap vInvN

  langN = lang{getCMap = cMapN, getCInv = cInvN, getVMap = vMapN, getVInv = vInvN, getRoots = rootsN, getManSyss = manSyssN, getRules = rule : getRules lang}

-- Creates a new vMap from a given vowel inventory
updateVMap :: [Phoneme] -> ([Height], [Backness], [Roundedness], [Length])
updateVMap vInv = (hs, bs, rs, ls) where
  hs = nub (map getHeight vInv)
  bs = nub (map getBackness vInv)
  rs = nub (map getRoundedness vInv)
  ls = nub (map getLength vInv)

-- Creates a new cMap from a given consonant inventory
updateCMap :: [Phoneme] -> ([Place], [Manner], [Phonation], [Airstream])
updateCMap cInv = (ps, ms, hs, as) where
  ps = nub (map getPlace cInv)
  ms = nub (map getManner cInv)
  hs = nub (map getVoice cInv)
  as = nub (map getAirstream cInv)

-- Applies a rule to each phoneme in multiple syllables
executeRuleOnSyllWord :: Int -> Rule -> [Syllable] -> [Syllable] -> ([Syllable], Int)
executeRuleOnSyllWord _ NoChange _ xs = (xs, 0)
executeRuleOnSyllWord i _ ys [] = (ys, i)
executeRuleOnSyllWord i rule@Rule{} ys (x:xs) = executeRuleOnSyllWord (i+j) rule (ys++[syllN]) xs where
  (syllN, j) = executeRuleOnSyllable rule x (lastMay ys, headMay xs)


-- Applies a phonological rule to each phoneme in a syllable
-- Applying the rule can result in new phonemes (though not impossible ones)
executeRuleOnSyllable :: Rule -> Syllable -> (Maybe Syllable, Maybe Syllable) -> (Syllable, Int)
executeRuleOnSyllable NoChange syll _ = (syll, 0)
executeRuleOnSyllable rule@Rule{} (Syllable onset nuc coda tone) (prev, next) = (Syllable onsetN nucN codaN tone, i+j+k) where
  (onsetN, i) = executeRuleOnOnset 0 rule [] onset (prevP, nuc)
  (nucN, j) = executeRuleOnNucleus rule nuc (prevP, onset, coda, nextP)
  (codaN, k) = executeRuleOnCoda 0 rule [] coda (nuc, nextP)
  prevP = syllToPhonemes <$> prev
  nextP = syllToPhonemes <$> next

syllToPhonemes :: Syllable -> [Phoneme]
syllToPhonemes (Syllable o n c _) = o ++ [n] ++ c

executeRuleOnOnset :: Int -> Rule -> [Phoneme] -> [Phoneme] -> (Maybe [Phoneme], Phoneme) -> ([Phoneme], Int)
executeRuleOnOnset _ NoChange _ xs _ = (xs, 0)
executeRuleOnOnset i _ ys [] _ = (ys, i)
executeRuleOnOnset i rule@(Rule a b p f) [] [x] (prev, nucleus) -- if only one phoneme in onset, compare using the nucleus and phoneme in previous syll
  | and [ comparePhonemeR p (join $ lastMay <$> prev) || comparePhonemeR2 p (join $ lastMay <$> prev)
        , comparePhonemeR (Just a) (Just x)
        , comparePhonemeR f (Just nucleus)
        , not $ impConsonants (applyRule b x)
        ] = executeRuleOnOnset (i+1) (Rule a b p f) [applyRule b x] [] (prev, nucleus)
  | otherwise = executeRuleOnOnset i (Rule a b p f) [x] [] (prev, nucleus)
executeRuleOnOnset i rule@(Rule a b p f) ys [x] (prev, nucleus) -- for the last phoneme in the onset, compare using the nucleus
  | and [ comparePhonemeR p (lastMay ys)
        , comparePhonemeR (Just a) (Just x)
        , comparePhonemeR f (Just nucleus)
        , not $ impConsonants (applyRule b x)
        ] = executeRuleOnOnset (i+1) (Rule a b p f) (ys++[applyRule b x]) [] (prev, nucleus)
  | otherwise = executeRuleOnOnset i (Rule a b p f) (ys++[x]) [] (prev, nucleus)
executeRuleOnOnset i rule@(Rule a b p f) [] (x:xs) (prev, nucleus) -- for the first phoneme in the onset, compare using the phonemes of the previous syllable
  | and [ comparePhonemeR p (join $ lastMay <$> prev) || comparePhonemeR2 p (join $ lastMay <$> prev)
        , comparePhonemeR (Just a) (Just x)
        , comparePhonemeR f (headMay xs)
        , not $ impConsonants (applyRule b x)
        ] = executeRuleOnOnset (i+1) (Rule a b p f) [applyRule b x] xs (prev, nucleus)
  | otherwise = executeRuleOnOnset i (Rule a b p f) [x] xs (prev, nucleus)
executeRuleOnOnset i rule@(Rule a b p f) ys (x:xs) s -- this looks at inter-onset phonemes
  | and [ comparePhonemeR p (lastMay ys)
        , comparePhonemeR (Just a) (Just x)
        , comparePhonemeR f (headMay xs)
        , not $ impConsonants (applyRule b x)
        ] = executeRuleOnOnset (i+1) (Rule a b p f) (ys++[applyRule b x]) xs s
  | otherwise = executeRuleOnOnset i (Rule a b p f) (ys++[x]) xs s

executeRuleOnCoda :: Int -> Rule -> [Phoneme] -> [Phoneme] -> (Phoneme, Maybe [Phoneme]) -> ([Phoneme], Int)
executeRuleOnCoda _ NoChange _ xs _ = (xs, 0)
executeRuleOnCoda i _ ys [] _ = (ys, i)
executeRuleOnCoda i rule@(Rule a b p f) [] [x] (nucleus, next)
  | and [ comparePhonemeR p (Just nucleus)
        , comparePhonemeR (Just a) (Just x)
        , comparePhonemeR f (join $ headMay <$> next) || comparePhonemeR2 f (join $ headMay <$> next)
        , not $ impConsonants (applyRule b x)
        ] = executeRuleOnCoda (i+1) (Rule a b p f) [applyRule b x] [] (nucleus, next)
  | otherwise = executeRuleOnCoda i (Rule a b p f) [x] [] (nucleus, next)
executeRuleOnCoda i rule@(Rule a b p f) ys [x] (nucleus, next)
  | and [ comparePhonemeR p (Just nucleus)
        , comparePhonemeR (Just a) (Just x)
        , comparePhonemeR f (headMay ys)
        , not $ impConsonants (applyRule b x)
        ] = executeRuleOnCoda (i+1) (Rule a b p f) (ys++[applyRule b x]) [] (nucleus, next)
  | otherwise = executeRuleOnCoda i (Rule a b p f) (ys++[x]) [] (nucleus, next)
executeRuleOnCoda i rule@(Rule a b p f) [] (x:xs) (nucleus, next)
  | and [ comparePhonemeR p (lastMay xs)
        , comparePhonemeR (Just a) (Just x)
        , comparePhonemeR f (join $ headMay <$> next) || comparePhonemeR2 f (join $ headMay <$> next)
        , not $ impConsonants (applyRule b x)
        ] = executeRuleOnCoda (i+1) (Rule a b p f) [applyRule b x] xs (nucleus, next)
  | otherwise = executeRuleOnCoda i (Rule a b p f) [x] xs (nucleus, next)
executeRuleOnCoda i rule@(Rule a b p f) ys (x:xs) s
  | and [ comparePhonemeR f (headMay ys)
        , comparePhonemeR (Just a) (Just x)
        , comparePhonemeR p (lastMay xs)
        , not $ impConsonants (applyRule b x)
        ] = executeRuleOnCoda (i+1) (Rule a b p f) (ys++[applyRule b x]) xs s
  | otherwise = executeRuleOnCoda i (Rule a b p f) (ys++[x]) xs s

executeRuleOnNucleus :: Rule -> Phoneme -> (Maybe [Phoneme], [Phoneme], [Phoneme], Maybe [Phoneme]) -> (Phoneme, Int)
executeRuleOnNucleus NoChange nuc _ = (nuc, 0)
executeRuleOnNucleus rule@(Rule a b p f) nuc (prev, [], [], next)
  | and [ comparePhonemeR p (join $ lastMay <$> prev) || comparePhonemeR2 p (join $ lastMay <$> prev)
        , comparePhonemeR (Just a) (Just nuc)
        , comparePhonemeR f (join $ headMay <$> next) || comparePhonemeR2 f (join $ headMay <$> next)
        , not $ impConsonants (applyRule b nuc)
        ] = (applyRule b nuc, 1)
  | otherwise = (nuc, 0)
executeRuleOnNucleus rule@(Rule a b p f) nuc (_, onset, [], next)
  | and [ comparePhonemeR p (lastMay onset)
        , comparePhonemeR (Just a) (Just nuc)
        , comparePhonemeR f (join $ headMay <$> next) || comparePhonemeR2 f (join $ headMay <$> next)
        , not $ impConsonants (applyRule b nuc)
        ] = (applyRule b nuc, 1)
  | otherwise = (nuc, 0)
executeRuleOnNucleus rule@(Rule a b p f) nuc (prev, [], coda, _)
  | and [ comparePhonemeR p (join $ lastMay <$> prev) || comparePhonemeR2 p (join $ lastMay <$> prev)
        , comparePhonemeR (Just a) (Just nuc)
        , comparePhonemeR f (headMay coda)
        , not $ impConsonants (applyRule b nuc)
        ] = (applyRule b nuc, 1)
  | otherwise = (nuc, 0)
executeRuleOnNucleus rule@(Rule a b p f) nuc (_, onset, coda, _)
  | and [ comparePhonemeR p (lastMay onset)
        , comparePhonemeR (Just a) (Just nuc)
        , comparePhonemeR f (headMay coda)
        , not $ impConsonants (applyRule b nuc)
        ] = (applyRule b nuc, 1)
  | otherwise = (nuc, 0)

-- Applies a phonological rule to each phoneme in a word/morpheme
-- Applying the rule can result in new phonemes (though not impossible ones)
executeRuleOnMorphWord :: Int -> Rule -> [Phoneme] -> [Phoneme] -> ([Phoneme], Int)
executeRuleOnMorphWord i _ ys [] = (ys, i)
executeRuleOnMorphWord i (Rule a b p f) ys (x:xs)
    | and [ comparePhonemeR p (lastMay ys)
          , comparePhonemeR (Just a) (Just x)
          , comparePhonemeR f (headMay xs)
          , not $ impConsonants (applyRule b x)
          ] = executeRuleOnMorphWord (i+1) (Rule a b p f) (ys++[applyRule b x]) xs
    | otherwise = executeRuleOnMorphWord i (Rule a b p f) (ys++[x]) xs
executeRuleOnMorphWord _ NoChange _ xs = (xs, 0)

-- Might need someone here if i do semivowel <-> vowel
applyRule :: PhonemeR -> Phoneme -> Phoneme
applyRule (ConsonantR pr mr hr ar) (Consonant p m h a) = Consonant (fromMaybe p pr) (fromMaybe m mr) (fromMaybe h hr) (fromMaybe a ar)
applyRule (VowelR hr br rr lr) (Vowel h b r l) = Vowel (fromMaybe h hr) (fromMaybe b br) (fromMaybe r rr) (fromMaybe l lr)
applyRule _ pho = pho --this should never happen


-- First argument refers to the Rule's pattern-phonemes
-- Nothing means "undefined" and matches everything
-- Second argument refres to the actual phoneme in the word
-- Nothing there means "Word Boundary" basically
-- This function only compares the phonemes themselves though
-- comparePhonemeR2 takes care of Syllable and Word Boundaries
comparePhonemeR :: Maybe PhonemeR -> Maybe Phoneme -> Bool
comparePhonemeR (Just (ConsonantR pr mr hr ar)) (Just (Consonant p m h a)) = fromMaybe True ((p ==) <$> pr)
                                                                          && fromMaybe True ((m ==) <$> mr)
                                                                          && fromMaybe True ((h ==) <$> hr)
                                                                          && fromMaybe True ((a ==) <$> ar)
comparePhonemeR (Just (VowelR hr br rr lr)) (Just (Vowel h b r l)) = fromMaybe True ((h ==) <$> hr)
                                                                  && fromMaybe True ((b ==) <$> br)
                                                                  && fromMaybe True ((r ==) <$> rr)
                                                                  && fromMaybe True ((l ==) <$> lr)
comparePhonemeR Nothing _ = True --undefined matches everything
comparePhonemeR _ _ = False

-- Special compare for Word and Syllable Boundaries
comparePhonemeR2 :: Maybe PhonemeR -> Maybe Phoneme -> Bool
comparePhonemeR2 (Just SyllableBoundary) _ = True -- if your using this function, you're comparing across syllable boundaries
comparePhonemeR2 (Just WordBoundary) Nothing = True -- finding nothing = you're looking at a word boundary
comparePhonemeR2 (Just WordBoundary) (Just _) = False -- finding anything = you're not looking at a word boundary
comparePhonemeR2 _ _ = False


-- Generate phonological Rule
generateRule :: Language -> RVar Rule
generateRule lang = do
  -- Phoneme A changes into Phoneme B...
  (soundA, soundB) <- join $ choice [ createCChangePattern lang
                                    , createCChangePattern2 lang
                                    , createVChangePattern lang
                                    , createVChangePattern2 lang
                                    ]

  -- ...preceding Phoneme P and following Phoneme F.
  (soundP, soundF) <- join $ choice [ createCEnvironmentPattern lang
                                    , createVEnvironmentPattern lang
                                    ]

  return $ Rule soundA soundB soundP soundF

-- [+consonant]__[+consonant] environment (only existing phonemes, of course)
createCEnvironmentPattern :: Language -> RVar (Maybe PhonemeR, Maybe PhonemeR)
createCEnvironmentPattern lang = do
  let (places, manners, phonations, airstreams) = getCMap lang
  prp <- join $ choice_ 1 Nothing <$> choice (map Just places)
  mrp <- join $ choice_ 1 Nothing <$> choice (map Just manners)
  hrp <- join $ choice_ 1 Nothing <$> choice (map Just phonations)
  arp <- join $ choice_ 1 Nothing <$> choice (map Just airstreams)

  prf <- join $ choice_ 1 Nothing <$> choice (map Just places)
  mrf <- join $ choice_ 1 Nothing <$> choice (map Just manners)
  hrf <- join $ choice_ 1 Nothing <$> choice (map Just phonations)
  arf <- join $ choice_ 1 Nothing <$> choice (map Just airstreams)

  p <- choice_ 4 (Just $ ConsonantR prp mrp hrp arp) (Just WordBoundary)
  f <- choice_ 4 (Just $ ConsonantR prf mrf hrf arf) (Just WordBoundary)
  return (p, f)

-- [+consonant] -> [+consonant] change (only uses existing p/m/h's)
createCChangePattern :: Language -> RVar (PhonemeR, PhonemeR)
createCChangePattern lang = do
  let (places, manners, phonations, airstreams) = getCMap lang
  pra <- join $ choice_ 1 Nothing <$> choice (map Just places)
  mra <- join $ choice_ 1 Nothing <$> choice (map Just manners)
  hra <- join $ choice_ 1 Nothing <$> choice (map Just phonations)
  ara <- join $ choice_ 1 Nothing <$> choice (map Just airstreams)

  prb <- join $ choice_ 1 Nothing <$> choice (Nothing : delete pra (map Just places))
  mrb <- join $ choice_ 1 Nothing <$> choice (Nothing : delete mra (map Just manners))
  hrb <- join $ choice_ 1 Nothing <$> choice (Nothing : delete hra (map Just phonations))
  arb <- join $ choice_ 1 Nothing <$> choice (map Just airstreams)
  return (ConsonantR pra mra hra ara, ConsonantR prb mrb hrb arb)

-- [+consonant] -> [+consonant] change (only uses unused p/m/h/a's)
createCChangePattern2 :: Language -> RVar (PhonemeR, PhonemeR)
createCChangePattern2 lang = do
  let (places, manners, phonations, airstreams) = getCMap lang
  pra <- join $ choice_ 1 Nothing <$> choice (map Just places)
  mra <- join $ choice_ 1 Nothing <$> choice (map Just manners)
  hra <- join $ choice_ 1 Nothing <$> choice (map Just phonations)
  ara <- join $ choice_ 1 Nothing <$> choice (map Just airstreams)

  let place
        | null ([BILABIAL .. GLOTTAL] \\ places) = return Nothing
        | otherwise = choice (map Just ([BILABIAL .. GLOTTAL] \\ places))

  prb <- join $ choice_ 1 Nothing <$> place

  let manner
        | null ([NASAL .. LFLAP] \\ manners) = return Nothing
        | otherwise = choice (map Just ([NASAL .. LFLAP] \\ manners))

  mrb <- join $ choice_ 1 Nothing <$> manner

  let phonation
        | null ([VOICELESS .. ASPIRATED] \\ phonations) = return Nothing
        | otherwise = choice (map Just ([VOICELESS .. ASPIRATED] \\ phonations))

  hrb <- join $ choice_ 1 Nothing <$> phonation

  let airstream
        | null ([PULMONIC .. LINGUAL] \\ airstreams) = return Nothing
        | otherwise = choice (map Just ([PULMONIC .. LINGUAL] \\ airstreams))

  arb <- join $ choice_ 1 Nothing <$> airstream

  return (ConsonantR pra mra hra ara, ConsonantR prb mrb hrb arb)


-- [+vowel]__[+vowel] environment (only existing phonemes, of course)
createVEnvironmentPattern :: Language -> RVar (Maybe PhonemeR, Maybe PhonemeR)
createVEnvironmentPattern lang = do
  let (heights, backs, rounds, lengths) = getVMap lang
  hrp <- join $ choice_ 2 Nothing <$> choice (map Just heights)
  brp <- join $ choice_ 2 Nothing <$> choice (map Just backs)
  rrp <- join $ choice_ 2 Nothing <$> choice (map Just rounds)
  lrp <- join $ choice_ 2 Nothing <$> choice (map Just lengths)

  hrf <- join $ choice_ 2 Nothing <$> choice (map Just heights)
  brf <- join $ choice_ 2 Nothing <$> choice (map Just backs)
  rrf <- join $ choice_ 2 Nothing <$> choice (map Just rounds)
  lrf <- join $ choice_ 2 Nothing <$> choice (map Just lengths)

  p <- choice_ 4 (Just $ VowelR hrp brp rrp lrp) (Just WordBoundary)
  f <- choice_ 4 (Just $ VowelR hrf brf rrf lrf) (Just WordBoundary)
  return (p, f)

-- [+vowel] -> [+vowel] change (only uses existing p/m/h's)
createVChangePattern :: Language -> RVar (PhonemeR, PhonemeR)
createVChangePattern lang = do
  let (heights, backs, rounds, lengths) = getVMap lang
  hra <- join $ choice_ 2 Nothing <$> choice (map Just heights)
  bra <- join $ choice_ 2 Nothing <$> choice (map Just backs)
  rra <- join $ choice_ 2 Nothing <$> choice (map Just rounds)
  lra <- join $ choice_ 2 Nothing <$> choice (map Just lengths)

  hrb <- join $ choice_ 2 Nothing <$> choice (Nothing : delete hra (map Just heights))
  brb <- join $ choice_ 2 Nothing <$> choice (Nothing : delete bra (map Just backs))
  rrb <- join $ choice_ 2 Nothing <$> choice (Nothing : delete rra (map Just rounds))
  lrb <- join $ choice_ 2 Nothing <$> choice (Nothing : delete lra (map Just lengths))
  return (VowelR hra bra rra lra, VowelR hrb brb rrb lrb)

-- [+vowel] -> [+vowel] change (only uses unused h/b/r/l/t's)
createVChangePattern2 :: Language -> RVar (PhonemeR, PhonemeR)
createVChangePattern2 lang = do
  let (heights, backs, rounds, lengths) = getVMap lang
  hra <- join $ choice_ 2 Nothing <$> choice (map Just heights)
  bra <- join $ choice_ 2 Nothing <$> choice (map Just backs)
  rra <- join $ choice_ 2 Nothing <$> choice (map Just rounds)
  lra <- join $ choice_ 2 Nothing <$> choice (map Just lengths)

  let height
        | null ([CLOSE .. OPEN] \\ heights) = return Nothing
        | otherwise = choice (map Just ([CLOSE .. OPEN] \\ heights))

  hrb <- join $ choice_ 2 Nothing <$> height

  let back
        | null ([BACK .. FRONT] \\ backs) = return Nothing
        | otherwise = choice (map Just ([BACK .. FRONT] \\ backs))

  brb <- join $ choice_ 2 Nothing <$> back

  let rnd
        | null ([ROUNDED .. UNROUNDED] \\ rounds) = return Nothing
        | otherwise = choice (map Just ([ROUNDED .. UNROUNDED] \\ rounds))

  rrb <- join $ choice_ 2 Nothing <$> rnd

  let lngth
        | null ([SHORT .. LONG] \\ lengths) = return Nothing
        | otherwise = choice (map Just ([SHORT .. LONG] \\ lengths))

  lrb <- join $ choice_ 2 Nothing <$> lngth

  return (VowelR hra bra rra lra, VowelR hrb brb rrb lrb)


-- Shelved
-- this should clean up any improper contrasts
-- like having short/long vowels, one of them needs to be redefined to 'normal'
-- maybe having only one of [LAMINALALVEOLAR, APICOALVEOLAR] should result in it redefining to ALVEOLAR
-- same with POSTALVEOLAR
-- having NEAROPEN without OPEN or NEARCLOSE without CLOSE seems like it should be impossible
-- same with NEARBACK and BACK and NEARFRONT and FRONT

-- so basically once a rule is applied and the cmap and vmaps are updated
-- check if there are "improper contrasts", and then go back through the lexicon and redefine shit
{-
cleanUpRuleC :: Language -> RVar Rule
cleanUpRuleC lang = langN where
  (ps, ms, hs) = getCMap langN
  action
    | LAMINALALVEOLAR `elem` ps && APICOALVEOLAR `notElem` ps = --LAMINALALVEOLAR -> ALVEOLAR
    | APICOALVEOLAR `elem` ps && LAMINALALVEOLAR `notElem` ps = --APICOALVEOLAR -> ALVEOLAR
    | ALVEOLAR `elem` ps && LAMINALALVEOLAR `elem` ps && APICOALVEOLAR `elem` ps = --ALVEOLAR -> LAMINALALVEOLAR or APICOALVEOLAR?
    | PALATOALVEOLAR `elem` ps && APICALRETROFLEX `notElem` ps && ALVEOLOPALATAL `notElem` ps = --PALATOALVEOLAR -> POSTALVEOLAR
    | APICALRETROFLEX `elem` ps && PALATOALVEOLAR `notElem` ps && ALVEOLOPALATAL `notElem` ps = --APICALRETROFLEX -> POSTALVEOLAR
    | ALVEOLOPALATAL `elem` ps && APICALRETROFLEX `notElem` ps && PALATOALVEOLAR `notElem` ps = --ALVEOLOPALATAL -> POSTALVEOLAR
    | POSTALVEOLAR `elem` ps && PALATOALVEOLAR `elem` ps && APICALRETROFLEX `notElem` ps && ALVEOLOPALATAL `notElem` ps = --POSTALVEOLAR -> APICALRETROFLEX or ALVEOLOPALATAL
    | POSTALVEOLAR `elem` ps && PALATOALVEOLAR `elem` ps && APICALRETROFLEX `elem` && ALVEOLOPALATAL `notElem` ps = --POSTALVEOLAR -> ALVEOLOPALATAL
    | POSTALVEOLAR `elem` ps && APICALRETROFLEX `elem` ps && PALATOALVEOLAR `notElem` ps && ALVEOLOPALATAL `notElem` ps = --POSTALVEOLAR -> PALATOALVEOLAR or ALVEOLOPALATAL
    | POSTALVEOLAR `elem` ps && APICALRETROFLEX `elem` ps && PALATOALVEOLAR `notElem` ps && ALVEOLOPALATAL `elem` ps = --POSTALVEOLAR -> PALATOALVEOLAR
    | POSTALVEOLAR `elem` ps && ALVEOLOPALATAL `elem` ps && PALATOALVEOLAR `notElem` ps && APICALRETROFLEX `notElem` ps = --POSTALVEOLAR -> PALATOALVEOLAR or APICALRETROFLEX
    | POSTALVEOLAR `elem` ps && ALVEOLOPALATAL `elem` ps && PALATOALVEOLAR `elem` ps && APICALRETROFLEX `notElem` ps = --POSTALVEOLAR -> APICALRETROFLEX
    | POSTALVEOLAR `elem` ps && ALVEOLOPALATAL `elem` ps && PALATOALVEOLAR `elem` ps && APICALRETROFLEX `elem` ps = --POSTALVEOLAR -> ANY

  return $ Rule soundA soundB soundP soundF

cleanUpRuleV :: Language -> RVar Rule
cleanUpRuleV lang = do
  (hs, bs, rs, ls, ts) = getVMap lanN
  actionV
    | LONG `elem` ls && SHORT `elem` ls && NORMAL `notElem` ls = --LONG or SHORT -> NORMAL
    | NEAROPEN `elem` hs && OPEN `notElem` hs = --NEAROPEN -> OPEN
    | NEARCLOSE `elem` hs && CLOSE `notElem` hs = --NEARCLOSE -> CLOSE
    | NEARBACK `elem` bs && BACK `notElem` bs = --NEARBACK -> BACK
    | NEARFRONT `elem` bs && FRONT `notElem` bs = --NEARFRONT -> FRONT
    | CLOSEMID `elem` hs && MID `notElem` hs && OPENMID `notElem` hs = --CLOSEMID -> MID
    | OPENMID `elem` hs && MID `notElem` hs && CLOSEMID `notElem` hs = --CLOSEMID -> MID
    | HRISET `elem` ts && LRISET `notElem` ts && RISET `notElem` ts = --HRISET -> RISET
    | LRISET `elem` ts && HRISET `notElem` ts && RISET `notElem` ts = --LRISET -> RISET
    | HFALLT `elem` ts && LFALLT `notElem` ts && FALLT `notElem` ts = --HFALLT -> FALLT
    | LFALLT `elem` ts && HFALLT `notElem` ts && FALLT `notElem` ts = --LFALLT -> FALLT

  return $ Rule soundA soundB soundP soundF
-}
