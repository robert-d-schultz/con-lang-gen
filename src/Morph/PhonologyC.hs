module Morph.PhonologyC
( morphPhonologyC
) where

import Data.RVar
import Data.Random.Extras
import Control.Monad
import Data.List

import HelperFunctions

import Data.Phoneme
import Data.Inflection
import Data.Other

import Gen.Phonology
import Gen.Phonotactics

-- "unconditioned merge"
morphPhonologyC :: Language -> RVar Language
morphPhonologyC lang = join $ choice [ morphPlace lang
                                     , morphManner lang
                                     ]

-- morph place of articulation
morphPlace :: Language -> RVar Language
morphPlace lang = do
  let (places,_,_) = getCMap lang
  let opts = [ (unconMergePlace [BILABIAL, LABIODENTAL] LABIAL lang, BILABIAL `elem` places)
             , (unconMergePlace [DENTIALVEOLAR, RETROFLEX] CORONAL lang, CORONAL `elem` places)
             , (unconMergePlace [DENTAL, ALVEOLAR, POSTALVEOLAR] DENTIALVEOLAR lang, DENTAL `elem` places)
             , (unconMergePlace [VELAR, UVULAR] DORSAL lang, VELAR `elem` places && UVULAR `elem` places && PALATAL `notElem` places)
             , (unconMergePlace [PALATAL, UVULAR] DORSAL lang, PALATAL `elem` places && UVULAR `elem` places && VELAR `notElem` places && ALVEOLOPALATAL `notElem` places)
             , (unconMergePlace [PALATAL, VELAR] DORSAL lang, PALATAL `elem` places && VELAR `elem` places && UVULAR `notElem` places && ALVEOLOPALATAL `notElem` places)
             , (unconMergePlace [PALATAL, VELAR, UVULAR] DORSAL lang, PALATAL `elem` places && VELAR `elem` places && UVULAR `elem` places && ALVEOLOPALATAL `notElem` places)
             , (unconMergePlace [EPIPHARYNGEAL, GLOTTAL] LARYNGEAL lang, EPIPHARYNGEAL `elem` places)
             , (unconMergePlace [PHARYNGEAL, EPIGLOTTAL] EPIPHARYNGEAL lang, PHARYNGEAL `elem` places)
             ]
  -- choose which change to do
  join $ choice $ map fst (filter snd opts)

-- merge two or more places of articulation
unconMergePlace :: [Place] -> Place -> Language -> RVar Language
unconMergePlace fromPlcs toPlc lang = do
  let (places, manners, phonations) = getCMap lang
  let roots = getRoots lang
  let manSyss = getManSyss lang

  -- merge places
  let placesN = toPlc : filter (`notElem` fromPlcs) places
  -- new consonant map
  let cMapN = (placesN, manners, phonations)
  --then for any phonemes with "fromPlcs" places replace with "toPlc"
  let rootsN = map (\(x, Morpheme y) -> (,) x (Morpheme $ map (unconMergePlaceDown fromPlcs toPlc) y)) roots
  -- same with inflection
  let manSyssN = map (\(ManifestSystem x y z) -> ManifestSystem x y (map (\(Morpheme w, v) -> (,) (Morpheme $ map (unconMergePlaceDown fromPlcs toPlc) w) v) z)) manSyss

  return lang{getCMap = cMapN, getRoots = rootsN, getManSyss = manSyssN}

-- used for unconditioned merging places
unconMergePlaceDown :: [Place] -> Place -> Phoneme -> Phoneme
unconMergePlaceDown fromPlcs toPlc (Consonant p m h)
  | p `elem` fromPlcs = Consonant toPlc m h
  | otherwise = Consonant p m h
unconMergePlaceDown _ _ x = x

-- used for unconditioned merging places
exceptUpdate :: [Place] -> Place -> [Phoneme] -> [Phoneme]
exceptUpdate _ _ [] = []
exceptUpdate fromPlcs toPlc e
  | cplace (head e) `elem` fromPlcs
    && all (\p -> (head e){cplace = p} `elem` tail e) (filter (/= cplace (head e)) fromPlcs) =
      (head e){cplace = toPlc} : exceptUpdate fromPlcs toPlc (filter (\x -> x `notElem` map (\p -> (head e){cplace = p}) (filter (/= cplace (head e)) fromPlcs)) (tail e))
  | otherwise = head e : exceptUpdate fromPlcs toPlc (tail e)


-- morph manner of articulation
morphManner :: Language -> RVar Language
morphManner lang = do
  let (_,manners,_) = getCMap lang
  let opts = [ (unconMergeManner [NASAL, STOP] STOP lang, all (`elem` manners) [NASAL, STOP])
             , (unconMergeManner [FLAP, STOP] FLAP lang, all (`elem` manners) [FLAP, STOP] && LFLAP `notElem` manners)
             , (unconMergeManner [TRILL, STOP] STOP lang, all (`elem` manners) [TRILL, STOP])
             , (unconMergeManner [LFLAP, FLAP] FLAP lang, all (`elem` manners) [LFLAP, FLAP])
             , (unconMergeManner [FRICATIVE, STOP] STOP lang, all (`elem` manners) [FRICATIVE, STOP] && all (`notElem` manners) [AFFRICATE, SILIBANT, LFRICATIVE])
             , (unconMergeManner [AFFRICATE, FRICATIVE] FRICATIVE lang, all (`elem` manners) [AFFRICATE, FRICATIVE] && all (`notElem` manners) [LAFFRICATE, SAFFRICATE])
             , (unconMergeManner [AFFRICATE, STOP] STOP lang, all (`elem` manners) [AFFRICATE, STOP] && all (`notElem` manners) [LAFFRICATE, SAFFRICATE])
             , (unconMergeManner [SILIBANT, FRICATIVE] FRICATIVE lang, all (`elem` manners) [SILIBANT, FRICATIVE])
             , (unconMergeManner [LFRICATIVE, FRICATIVE] FRICATIVE lang, all (`elem` manners) [LFRICATIVE, FRICATIVE])
             , (unconMergeManner [LAFFRICATE, AFFRICATE] AFFRICATE lang, all (`elem` manners) [LAFFRICATE, AFFRICATE])
             , (unconMergeManner [SAFFRICATE, AFFRICATE] AFFRICATE lang, all (`elem` manners) [SAFFRICATE, AFFRICATE])
             , (unconMergeManner [LAPPROXIMANT, APPROXIMANT] APPROXIMANT lang, all (`elem` manners) [LAPPROXIMANT, APPROXIMANT])
             -- off the rails
             , (unconMergeManner [LAFFRICATE, LFRICATIVE] LFRICATIVE lang, all (`elem` manners) [LAFFRICATE, LFRICATIVE])
             , (unconMergeManner [SAFFRICATE, SILIBANT] SILIBANT lang, all (`elem` manners) [SAFFRICATE, SILIBANT])
             , (unconMergeManner [LAPPROXIMANT, LFRICATIVE] LFRICATIVE lang, all (`elem` manners) [LAPPROXIMANT, LFRICATIVE])
             , (unconMergeManner [APPROXIMANT, FRICATIVE] FRICATIVE lang, all (`elem` manners) [APPROXIMANT, FRICATIVE] && all (`notElem` manners) [LFRICATIVE, SILIBANT])
             , (unconMergeManner [TRILL, FLAP] FLAP lang, all (`elem` manners) [TRILL, FLAP])
             , (unconMergeManner [TRILL, FLAP] TRILL lang, all (`elem` manners) [TRILL, FLAP] && LFLAP `notElem` manners)
             ]

  -- choose which change to do
  join $ choice $ map fst (filter snd opts)

-- merge two or more manners of articulation
unconMergeManner :: [Manner] -> Manner -> Language -> RVar Language
unconMergeManner fromMans toMan lang = do
  let (places, manners, phonations) = getCMap lang
  let roots = getRoots lang
  let manSyss = getManSyss lang

  -- merge manners
  let mannersN = toMan : filter (`notElem` fromMans) manners
  -- new consonant map
  let cMapN = (places, mannersN, phonations)
  --then for any phonemes with "fromMans" places replace with "toMan"
  let rootsN = map (\(x, Morpheme y) -> (,) x (Morpheme $ map (unconMergeMannerDown fromMans toMan) y)) roots
  -- same with inflection
  let manSyssN = map (\(ManifestSystem x y z) -> ManifestSystem x y (map (\(Morpheme w, v) -> (,) (Morpheme $ map (unconMergeMannerDown fromMans toMan) w) v) z)) manSyss

  return lang{getCMap = cMapN, getRoots = rootsN, getManSyss = manSyssN}

-- used for unconditioned merging manners
unconMergeMannerDown :: [Manner] -> Manner -> Phoneme -> Phoneme
unconMergeMannerDown fromMans toMan (Consonant p m h)
  | m `elem` fromMans = Consonant p toMan h
  | otherwise = Consonant p m h
unconMergeMannerDown _ _ x = x

-- used for unconditioned merging manners
exceptUpdate2 :: [Manner] -> Manner -> [Phoneme] -> [Phoneme]
exceptUpdate2 _ _ [] = []
exceptUpdate2 fromMans toMan e
  | cmanner (head e) `elem` fromMans
    && all (\m -> (head e){cmanner = m} `elem` tail e) (filter (/= cmanner (head e)) fromMans) =
      (head e){cmanner = toMan} : exceptUpdate2 fromMans toMan (filter (\x -> x `notElem` map (\m -> (head e){cmanner = m}) (filter (/= cmanner (head e)) fromMans)) (tail e))
  | otherwise = head e : exceptUpdate2 fromMans toMan (tail e)
