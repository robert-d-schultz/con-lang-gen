{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
module Morph.Phonology
( phonologicalChange
) where

import ClassyPrelude hiding ((\\))

import Data.RVar
import Data.Random.Extras
import Data.List (nub, (\\))

import Data.Phoneme
import Data.Inflection
import Data.Other
import Data.Soundchange

import Constants
import HelperFunctions

-- need a way of guanrenteeing a sound change
-- so like there needs to be a loop of generating and checking rules
-- generate a rule randomly, then execute it on the lexicon, then check how many things changed
-- if enough things chang (like 2 things per a rule should be the absolute minimum), then continue on, otherwise reject
-- then repeat
-- and cancel after like 50 attempts, I guess

-- given a phonological rule, change some phonemes into previously-not-existing ones
phonologicalChange :: Int -> Language -> RVar Language
phonologicalChange 50 lang = trace "Gave up on sound change" return lang --give up after 50 attempts
phonologicalChange i lang = do
  rule <- generateRule lang
  let (langN, changes) = executeRuleOnLanguage rule lang
  if changes < 2 then phonologicalChange (i+1) lang else trace (show rule ++ ", " ++ show changes) (return langN)

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

executeRuleOnLanguage :: Rule -> Language -> (Language, Int)
executeRuleOnLanguage rule lang = (langN, rootChanges + inflChanges) where

  -- execute rule on each (valid) phoneme in the lexicon and inflection system
  roots = getRoots lang
  manSyss = getManSyss lang

  (rootsN, rootChanges) = second sum $ unzip $ map (\(x, Morpheme y) -> first ((,) x . Morpheme) (executeRuleOnWord 0 lang rule [] y)) roots :: ([((Text, LexCat), Morpheme)], Int)
  (manSyssN, inflChanges) = second sum $ unzip $ map (\(ManifestSystem x y z) -> first (ManifestSystem x y) (second sum $ unzip $ map (\(Morpheme w, v) -> first (swap . (,) v . Morpheme) (executeRuleOnWord 0 lang rule [] w)) z)) manSyss :: ([ManifestSystem], Int)

  -- take a new survey of which phonemes exist in the lexicon
  -- update inventories and maps
  lexPhonemes = nub (concatMap (\(_, Morpheme y) -> y) rootsN)
  declPhonemes = concatMap (\(ManifestSystem _ _ z) -> (concatMap (\(Morpheme w, _) -> w) z)) manSyssN
  allPhonemes = nub $ lexPhonemes ++ declPhonemes
  cInvN = filter isConsonant allPhonemes
  vInvN = filter isVowel allPhonemes
  cMapN = updateCMap cInvN
  vMapN = updateVMap vInvN

  langN = lang{getCMap = cMapN, getCInv = cInvN, getVMap = vMapN, getVInv = vInvN, getRoots = rootsN, getManSyss = manSyssN}

-- creates a new vMap from a given vowel inventory
updateVMap :: [Phoneme] -> ([Height], [Backness], [Roundedness], [Length], [Tone])
updateVMap vInv = (hs, bs, rs, ls, ts) where
  hs = nub (map vheight vInv)
  bs = nub (map vbackness vInv)
  rs = nub (map vroundedness vInv)
  ls = nub (map vlength vInv)
  ts = nub (map vtone vInv)

-- creates a new cMap from a given consonant inventory
updateCMap :: [Phoneme] -> ([Place], [Manner], [Phonation])
updateCMap cInv = (ps, ms, hs) where
  ps = nub (map cplace cInv)
  ms = nub (map cmanner cInv)
  hs = nub (map cvoice cInv)

-- applies a phonological rule to each phoneme in a word/morpheme
-- applying the rule CAN result in new phonemes (though not impossible ones)
executeRuleOnWord :: Int -> Language -> Rule -> [Phoneme] -> [Phoneme] -> ([Phoneme], Int)
executeRuleOnWord i _ _ ys [] = (ys, i)
executeRuleOnWord i l (Rule a b p f) ys (x:xs)
    | and [ comparePhonemeR p (lastMay ys)
          , comparePhonemeR (Just a) (Just x)
          , comparePhonemeR f (headMay xs)
          , not $ impConsonants (applyRule b x)
          ] = executeRuleOnWord (i+1) l (Rule a b p f) (ys++[applyRule b x]) xs
    | otherwise = executeRuleOnWord i l (Rule a b p f) (ys++[x]) xs

-- might need someone here if i do semivowel <-> vowel
applyRule :: PhonemeR -> Phoneme -> Phoneme
applyRule (ConsonantR pr mr hr) (Consonant p m h) = Consonant (fromMaybe p pr) (fromMaybe m mr) (fromMaybe h hr)
applyRule (VowelR hr br rr lr tr) (Vowel h b r l t) = Vowel (fromMaybe h hr) (fromMaybe b br) (fromMaybe r rr) (fromMaybe l lr) (fromMaybe t tr)
applyRule _ pho = pho --this should never happen

comparePhonemeR :: Maybe PhonemeR -> Maybe Phoneme -> Bool
comparePhonemeR (Just (ConsonantR pr mr hr)) (Just (Consonant p m h)) = fromMaybe True ((p ==) <$> pr)
                                                                     && fromMaybe True ((m ==) <$> mr)
                                                                     && fromMaybe True ((h ==) <$> hr)
comparePhonemeR (Just (VowelR hr br rr lr tr)) (Just (Vowel h b r l t)) = fromMaybe True ((h ==) <$> hr)
                                                                       && fromMaybe True ((b ==) <$> br)
                                                                       && fromMaybe True ((r ==) <$> rr)
                                                                       && fromMaybe True ((l ==) <$> lr)
                                                                       && fromMaybe True ((t ==) <$> tr)
comparePhonemeR (Just WordBoundary) Nothing = True --match word boundary
comparePhonemeR Nothing Nothing = False --undefined phoneme /= word boundary
comparePhonemeR Nothing _ = True --undefined phoneme matches everything
comparePhonemeR _ _ = False

-- generate phonological rule
generateRule :: Language -> RVar Rule
generateRule lang = do
  -- phoneme A changes into phoneme B...
  (soundA, soundB) <- join $ choice [ createCChangePattern lang
                                    , createCChangePattern2 lang
                                    , createVChangePattern lang
                                    , createVChangePattern2 lang
                                    ]

  -- ...preceding phoneme P and following phoneme F.
  (soundP, soundF) <- join $ choice [ createCEnvironmentPattern lang
                                    , createVEnvironmentPattern lang
                                    ]

  return $ Rule soundA soundB soundP soundF

-- [+consonant]__[+consonant] environment (only existing phonemes, of course)
createCEnvironmentPattern :: Language -> RVar (Maybe PhonemeR, Maybe PhonemeR)
createCEnvironmentPattern lang = do
  let (places, manners, phonations) = getCMap lang
  prp <- join $ choice_ 1 Nothing <$> choice (map Just places)
  mrp <- join $ choice_ 1 Nothing <$> choice (map Just manners)
  hrp <- join $ choice_ 1 Nothing <$> choice (map Just phonations)

  prf <- join $ choice_ 1 Nothing <$> choice (map Just places)
  mrf <- join $ choice_ 1 Nothing <$> choice (map Just manners)
  hrf <- join $ choice_ 1 Nothing <$> choice (map Just phonations)

  p <- choice_ 4 (Just $ ConsonantR prp mrp hrp) (Just WordBoundary)
  f <- choice_ 4 (Just $ ConsonantR prf mrf hrf) (Just WordBoundary)
  return (p, f)

-- [+consonant] -> [+consonant] change (only uses existing p/m/h's)
createCChangePattern :: Language -> RVar (PhonemeR, PhonemeR)
createCChangePattern lang = do
  let (places, manners, phonations) = getCMap lang
  pra <- join $ choice_ 1 Nothing <$> choice (map Just places)
  mra <- join $ choice_ 1 Nothing <$> choice (map Just manners)
  hra <- join $ choice_ 1 Nothing <$> choice (map Just phonations)

  prb <- join $ choice_ 1 Nothing <$> choice (Nothing : delete pra (map Just places))
  mrb <- join $ choice_ 1 Nothing <$> choice (Nothing : delete mra (map Just manners))
  hrb <- join $ choice_ 1 Nothing <$> choice (Nothing : delete hra (map Just phonations))
  return (ConsonantR pra mra hra, ConsonantR prb mrb hrb)

-- [+consonant] -> [+consonant] change (only uses unused p/m/h's)
createCChangePattern2 :: Language -> RVar (PhonemeR, PhonemeR)
createCChangePattern2 lang = do
  let (places, manners, phonations) = getCMap lang
  pra <- join $ choice_ 1 Nothing <$> choice (map Just places)
  mra <- join $ choice_ 1 Nothing <$> choice (map Just manners)
  hra <- join $ choice_ 1 Nothing <$> choice (map Just phonations)

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

  return (ConsonantR pra mra hra, ConsonantR prb mrb hrb)


-- [+vowel]__[+vowel] environment (only existing phonemes, of course)
createVEnvironmentPattern :: Language -> RVar (Maybe PhonemeR, Maybe PhonemeR)
createVEnvironmentPattern lang = do
  let (heights, backs, rounds, lengths, tones) = getVMap lang
  hrp <- join $ choice_ 2 Nothing <$> choice (map Just heights)
  brp <- join $ choice_ 2 Nothing <$> choice (map Just backs)
  rrp <- join $ choice_ 2 Nothing <$> choice (map Just rounds)
  lrp <- join $ choice_ 2 Nothing <$> choice (map Just lengths)
  trp <- join $ choice_ 2 Nothing <$> choice (map Just tones)

  hrf <- join $ choice_ 2 Nothing <$> choice (map Just heights)
  brf <- join $ choice_ 2 Nothing <$> choice (map Just backs)
  rrf <- join $ choice_ 2 Nothing <$> choice (map Just rounds)
  lrf <- join $ choice_ 2 Nothing <$> choice (map Just lengths)
  trf <- join $ choice_ 2 Nothing <$> choice (map Just tones)

  p <- choice_ 4 (Just $ VowelR hrp brp rrp lrp trp) (Just WordBoundary)
  f <- choice_ 4 (Just $ VowelR hrf brf rrf lrf trf) (Just WordBoundary)
  return (p, f)

-- [+vowel] -> [+vowel] change (only uses existing p/m/h's)
createVChangePattern :: Language -> RVar (PhonemeR, PhonemeR)
createVChangePattern lang = do
  let (heights, backs, rounds, lengths, tones) = getVMap lang
  hra <- join $ choice_ 2 Nothing <$> choice (map Just heights)
  bra <- join $ choice_ 2 Nothing <$> choice (map Just backs)
  rra <- join $ choice_ 2 Nothing <$> choice (map Just rounds)
  lra <- join $ choice_ 2 Nothing <$> choice (map Just lengths)
  tra <- join $ choice_ 2 Nothing <$> choice (map Just tones)

  hrb <- join $ choice_ 2 Nothing <$> choice (Nothing : delete hra (map Just heights))
  brb <- join $ choice_ 2 Nothing <$> choice (Nothing : delete bra (map Just backs))
  rrb <- join $ choice_ 2 Nothing <$> choice (Nothing : delete rra (map Just rounds))
  lrb <- join $ choice_ 2 Nothing <$> choice (Nothing : delete lra (map Just lengths))
  trb <- join $ choice_ 2 Nothing <$> choice (Nothing : delete tra (map Just tones))
  return (VowelR hra bra rra lra tra, VowelR hrb brb rrb lrb trb)


-- [+vowel] -> [+vowel] change (only uses unused h/b/r/l/t's)
createVChangePattern2 :: Language -> RVar (PhonemeR, PhonemeR)
createVChangePattern2 lang = do
  let (heights, backs, rounds, lengths, tones) = getVMap lang
  hra <- join $ choice_ 2 Nothing <$> choice (map Just heights)
  bra <- join $ choice_ 2 Nothing <$> choice (map Just backs)
  rra <- join $ choice_ 2 Nothing <$> choice (map Just rounds)
  lra <- join $ choice_ 2 Nothing <$> choice (map Just lengths)
  tra <- join $ choice_ 2 Nothing <$> choice (map Just tones)

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

  let tone
        | null ([NONET .. PEAKT] \\ tones) = return Nothing
        | otherwise = choice (map Just ([NONET .. PEAKT] \\ tones))

  trb <- join $ choice_ 2 Nothing <$> tone

  return (VowelR hra bra rra lra tra, VowelR hrb brb rrb lrb trb)
