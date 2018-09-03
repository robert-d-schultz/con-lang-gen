module Constants
( impConsonants
, mannerDistance
, placeDistance
) where

import ClassyPrelude

import Data.Phoneme

-- Impossible consonants filter
-- True = impossible
impConsonants :: Phoneme -> Bool
impConsonants c@(Consonant p m h a)
  -- Silibants are restricted to these Places
  | p `notElem` [DENTIALVEOLAR, DENTAL, LAMINALALVEOLAR, APICOALVEOLAR, PALATOALVEOLAR, APICALRETROFLEX, ALVEOLOPALATAL, RETROFLEX] && m `elem` [SILIBANT, SAFFRICATE] = True
  -- Laterals are restricted from these Places
  | p `elem` [BILABIAL, LABIODENTAL, EPIPHARYNGEAL, PHARYNGEAL, EPIGLOTTAL, GLOTTAL] && m `elem` [LAFFRICATE, LAPPROXIMANT, LFRICATIVE, LFRICATIVE, LFLAP] = True
  -- Flaps and Trills are restricted from these Places
  | p `elem` [ALVEOLOPALATAL, PALATAL, VELAR, GLOTTAL] && m `elem` [FLAP, TRILL] = True
  -- Only *Voiceless* Stops/Affricates are allowed in these Places
  | p `elem` [EPIPHARYNGEAL, PHARYNGEAL, GLOTTAL] && m `elem` [STOP, AFFRICATE] && h /= VOICELESS = True
  -- Only Stops, Affricates, and Clicks can be Aspirated
  | m `notElem` [STOP, AFFRICATE, SAFFRICATE, LAFFRICATE, CLICK] && h == ASPIRATED = True
  -- Nasals are restricted from these Places
  | p `elem` [EPIPHARYNGEAL, PHARYNGEAL, EPIGLOTTAL, GLOTTAL] && m == NASAL = True
  -- Fricatives and Approximants happen Pharyngeral-ly (or Epiglotto-pharyngeal-ly)
  | p == EPIGLOTTAL && m `elem` [FRICATIVE, APPROXIMANT, AFFRICATE] = True
  -- Stops, Trills, and Flaps happen Epiglottal-ly (or Epiglotto-pharyngeal-ly)
  | p == PHARYNGEAL && m `elem` [STOP, TRILL, FLAP, AFFRICATE] = True
  -- Ejectives are basically always Voiceless
  | h /= VOICELESS && a == EJECTIVE = True
  -- Ejectives are always Obstruents (No Silibants though?)
  | m `notElem` [STOP, AFFRICATE, FRICATIVE, LAFFRICATE, LFRICATIVE] && a == EJECTIVE = True
  -- Implosives are basically always Stops
  | m /= STOP && a == IMPLOSIVE = True
  -- Implosives are basically never anything past Uvular
  | a == IMPLOSIVE && p `elem` [PHARYNGEAL, EPIPHARYNGEAL, EPIGLOTTAL, GLOTTAL] = True
  -- Clicks are never anything but Lingual Ingressive and vice versa
  | m == CLICK && a /= LINGUAL || m /= CLICK && a == LINGUAL = True
  -- Clicks are restricted from these Places
  | m == CLICK && p `elem` [VELAR, UVULAR, PHARYNGEAL, EPIPHARYNGEAL, EPIGLOTTAL, GLOTTAL] = True
  -- Clicks are restricted to these Phonations
  | m == CLICK && h `notElem` [MODAL, VOICELESS, BREATHY, ASPIRATED] = True
  -- Check coarticulated
  | (\case COARTICULATED{} -> True; _ -> False) p = impCoarticulated c
  -- Otherwise, false
  | otherwise = False
impConsonants _ = False


impCoarticulated :: Phoneme -> Bool
impCoarticulated (Consonant (COARTICULATED p1 p2) m h a)
  -- Let's not do triple/quadruple articulation...
  | (\case COARTICULATED{} -> True; _ -> False) p1 || (\case COARTICULATED{} -> True; _ -> False) p2 = True
  -- Coarticulated constants can't have the same POA, that'd be dumb
  | p1 == p2 = True
  -- Keep places in order (front to back)
  | p1 > p2 = True
  -- If either Place is impossible, the whole thing is
  | impConsonants (Consonant p1 m h a) || impConsonants (Consonant p2 m h a) = True
  -- Coarticulated constants are restricted to these Manners
  | m `notElem` [NASAL, STOP, APPROXIMANT, LAPPROXIMANT] = True
  -- Glottis is not considered an articulator in this case
  | p1 == GLOTTAL || p2 == GLOTTAL = True
  -- Restrictions on Coart Stops: Too close together
  | m `elem` [STOP, NASAL] && p1 == BILABIAL && p2 == INTERDENTAL = True
  -- (Bi)Labial-Dorsal Stops are fine
  | m `elem` [STOP, NASAL] && p1 == BILABIAL && p2 `elem` [ALVEOLOPALATAL, PALATAL, VELAR, UVULAR] = False
  -- Other Labial-Dorsal's are too rare
  | m `elem` [STOP, NASAL] && p1 `elem` [LABIODENTAL, LINGUOLABIAL] && p2 `elem` [ALVEOLOPALATAL, PALATAL, VELAR, UVULAR] = True
  -- Labial-Coronal is too rare
  | m `elem` [STOP, NASAL] && p1 `elem` [BILABIAL, LABIODENTAL, LINGUOLABIAL] && p2 `elem` [INTERDENTAL, DENTAL, DENTIALVEOLAR, LAMINALALVEOLAR, APICOALVEOLAR, PALATOALVEOLAR, APICALRETROFLEX, RETROFLEX] = True
  -- Labial-Pharyngeal is unheard of
  | m `elem` [STOP, NASAL] && p1 `elem` [BILABIAL, LABIODENTAL, LINGUOLABIAL] && p2 `elem` [PHARYNGEAL, EPIPHARYNGEAL, EPIGLOTTAL, GLOTTAL] = True
  -- Coronal-Dorsal is too rare
  | m `elem` [STOP, NASAL] && p1 `elem` [INTERDENTAL, DENTAL, DENTIALVEOLAR, LAMINALALVEOLAR, APICOALVEOLAR, PALATOALVEOLAR, APICALRETROFLEX, RETROFLEX] && p2 `elem` [ALVEOLOPALATAL, PALATAL, VELAR, UVULAR] = True
  -- Coronal-Pharyngeal is unheard of
  | m `elem` [STOP, NASAL] && p1 `elem` [INTERDENTAL, DENTAL, DENTIALVEOLAR, LAMINALALVEOLAR, APICOALVEOLAR, PALATOALVEOLAR, APICALRETROFLEX, RETROFLEX] && p2 `elem` [PHARYNGEAL, EPIPHARYNGEAL, EPIGLOTTAL, GLOTTAL] = True
  -- Dorsal-Pharyngeal is too rare
  | m `elem` [STOP, NASAL] && p1 `elem` [ALVEOLOPALATAL, PALATAL, VELAR, UVULAR] && p2 `elem` [PHARYNGEAL, EPIPHARYNGEAL, EPIGLOTTAL, GLOTTAL] = True
  -- Otherwise, False
  | otherwise = False


-- Calculates an approximate similarity between two phonemes
-- Max of ~30
phonemeDistance :: Phoneme -> Phoneme -> Int
--distances bewteen two vowels (somewhat manhatten distance)
phonemeDistance (Vowel h1 b1 r1 l1) (Vowel h2 b2 r2 l2) = 2 * abs (fromEnum h1 - fromEnum h2)
                                                            + 2 * abs (fromEnum b1 - fromEnum b2)
                                                            + 5 * abs (fromEnum r1 - fromEnum r2)
                                                            + 3 * abs (fromEnum l1 - fromEnum l2)
-- Distances between two consonants (somewhat manhatten distance)
phonemeDistance (Consonant p1 m1 h1 a1) (Consonant p2 m2 h2 a2) = 2 * placeDistance p1 p2
                                                          + 2 * mannerDistance m1 m2
                                                          + 1 * phonationDistance h1 h2
                                                          + 1 * airstreamDistance a1 a2
-- Distances a vowel and diphthong (average distance from vowel to the two end points)
phonemeDistance (vow@Vowel{}) (Diphthong b2 r2 h2 b3 r3 h3 l2) = truncate result where
  result = realToFrac (phonemeDistance vow (Vowel b2 r2 h2 l2) + phonemeDistance vow (Vowel b3 r3 h3 l2)) / 2
phonemeDistance (diph@Diphthong{}) (vow@Vowel{}) = phonemeDistance vow diph
-- Distance between two diphthongs (cosine distance)
phonemeDistance (Diphthong b1 r1 h1 b2 h2 r2 l1) (Diphthong b3 r3 h3 b4 r4 h4 l2) = round (24 * cosDist) + 3 * abs (fromEnum l1 - fromEnum l2) where
  dp = ((fromEnum h1 - fromEnum h2) * (fromEnum h3 - fromEnum h4))
     + ((fromEnum b1 - fromEnum b2) * (fromEnum b3 - fromEnum b4))
     + ((fromEnum r1 - fromEnum r2) * (fromEnum r3 - fromEnum r4))
  mags = sqrt (fromIntegral ((fromEnum h1 - fromEnum h2) ^ 2 + (fromEnum b1 - fromEnum b2) ^ 2 + (fromEnum r1 - fromEnum r2) ^ 2))
       * sqrt (fromIntegral ((fromEnum h3 - fromEnum h4) ^ 2 + (fromEnum b3 - fromEnum b4) ^ 2 + (fromEnum r3 - fromEnum r4) ^ 2))
  cosDist = realToFrac dp / realToFrac mags
-- Semivowels...
phonemeDistance (Consonant PALATAL APPROXIMANT MODAL PULMONIC) (Vowel CLOSE BACK UNROUNDED _ ) = 10
phonemeDistance (Vowel CLOSE BACK UNROUNDED _) (Consonant PALATAL APPROXIMANT MODAL PULMONIC) = 10
phonemeDistance (Consonant VELAR APPROXIMANT MODAL PULMONIC) (Vowel CLOSE BACK ROUNDED _) = 10
phonemeDistance (Vowel CLOSE BACK ROUNDED _) (Consonant VELAR APPROXIMANT MODAL PULMONIC) = 10
phonemeDistance (Consonant PHARYNGEAL APPROXIMANT MODAL PULMONIC) (Vowel OPEN BACK UNROUNDED _) = 10
phonemeDistance (Vowel OPEN BACK UNROUNDED _) (Consonant PHARYNGEAL APPROXIMANT MODAL PULMONIC) = 10
-- Everything else
phonemeDistance _ _ = 60


mannerDistance :: Manner -> Manner -> Int
mannerDistance NASAL STOP = 1
mannerDistance STOP SAFFRICATE = 1
mannerDistance STOP AFFRICATE = 1
mannerDistance STOP SILIBANT = 3
mannerDistance STOP FRICATIVE = 3
mannerDistance STOP FLAP = 2
mannerDistance STOP TRILL = 3
mannerDistance STOP LAFFRICATE = 1
mannerDistance STOP LFLAP = 2
mannerDistance SAFFRICATE AFFRICATE = 1
mannerDistance SAFFRICATE SILIBANT = 1
mannerDistance SAFFRICATE LAFFRICATE = 1
mannerDistance AFFRICATE LAFFRICATE = 1
mannerDistance AFFRICATE FRICATIVE = 1
mannerDistance SILIBANT FRICATIVE = 1
mannerDistance SILIBANT APPROXIMANT = 2
mannerDistance FRICATIVE APPROXIMANT = 1
mannerDistance FRICATIVE LFRICATIVE = 1
mannerDistance APPROXIMANT LAPPROXIMANT = 1
mannerDistance FLAP LFLAP = 1
mannerDistance STOP NASAL = 1
mannerDistance SAFFRICATE STOP = 1
mannerDistance AFFRICATE STOP = 1
mannerDistance SILIBANT STOP = 3
mannerDistance FRICATIVE STOP = 3
mannerDistance FLAP STOP = 2
mannerDistance TRILL STOP = 3
mannerDistance LAFFRICATE STOP = 1
mannerDistance LFLAP STOP = 2
mannerDistance AFFRICATE SAFFRICATE = 1
mannerDistance SILIBANT SAFFRICATE = 1
mannerDistance LAFFRICATE SAFFRICATE = 1
mannerDistance LAFFRICATE AFFRICATE = 1
mannerDistance FRICATIVE AFFRICATE= 1
mannerDistance FRICATIVE SILIBANT = 1
mannerDistance APPROXIMANT SILIBANT = 2
mannerDistance APPROXIMANT FRICATIVE = 1
mannerDistance LFRICATIVE FRICATIVE = 1
mannerDistance LAPPROXIMANT APPROXIMANT = 1
mannerDistance LFLAP FLAP = 1
mannerDistance x y | x == y = 0
                   | otherwise = 5

placeDistance :: Place -> Place -> Int
placeDistance BILABIAL LABIODENTAL = 2
placeDistance BILABIAL LINGUOLABIAL = 3
placeDistance LABIODENTAL INTERDENTAL = 3
placeDistance LINGUOLABIAL INTERDENTAL = 2
placeDistance INTERDENTAL DENTIALVEOLAR = 2
placeDistance INTERDENTAL DENTAL = 2
placeDistance DENTIALVEOLAR LAMINALALVEOLAR = 2
placeDistance DENTIALVEOLAR DENTAL = 1
placeDistance LAMINALALVEOLAR APICOALVEOLAR = 1
placeDistance LAMINALALVEOLAR PALATOALVEOLAR = 2
placeDistance PALATOALVEOLAR APICALRETROFLEX = 1
placeDistance PALATOALVEOLAR ALVEOLOPALATAL = 3
placeDistance APICOALVEOLAR APICALRETROFLEX = 1
placeDistance APICALRETROFLEX RETROFLEX = 2
placeDistance APICALRETROFLEX ALVEOLOPALATAL = 2
placeDistance RETROFLEX ALVEOLOPALATAL = 3
placeDistance RETROFLEX PALATAL = 3
placeDistance ALVEOLOPALATAL PALATAL = 2
placeDistance PALATAL VELAR = 2
placeDistance VELAR UVULAR = 2
placeDistance UVULAR PHARYNGEAL = 3
placeDistance PHARYNGEAL EPIPHARYNGEAL = 1
placeDistance EPIPHARYNGEAL EPIGLOTTAL = 1
placeDistance EPIGLOTTAL GLOTTAL = 2
placeDistance LABIODENTAL BILABIAL = 2
placeDistance LINGUOLABIAL BILABIAL = 3
placeDistance INTERDENTAL LABIODENTAL = 3
placeDistance INTERDENTAL LINGUOLABIAL = 2
placeDistance DENTIALVEOLAR INTERDENTAL = 2
placeDistance DENTAL INTERDENTAL = 2
placeDistance LAMINALALVEOLAR DENTIALVEOLAR = 2
placeDistance DENTAL DENTIALVEOLAR = 1
placeDistance APICOALVEOLAR LAMINALALVEOLAR = 1
placeDistance PALATOALVEOLAR LAMINALALVEOLAR = 2
placeDistance APICALRETROFLEX PALATOALVEOLAR = 1
placeDistance ALVEOLOPALATAL PALATOALVEOLAR = 3
placeDistance APICALRETROFLEX APICOALVEOLAR = 1
placeDistance RETROFLEX APICALRETROFLEX = 2
placeDistance ALVEOLOPALATAL APICALRETROFLEX = 2
placeDistance ALVEOLOPALATAL RETROFLEX = 3
placeDistance PALATAL RETROFLEX = 3
placeDistance PALATAL ALVEOLOPALATAL = 2
placeDistance VELAR PALATAL = 2
placeDistance UVULAR VELAR = 2
placeDistance PHARYNGEAL UVULAR = 3
placeDistance EPIPHARYNGEAL PHARYNGEAL = 1
placeDistance EPIGLOTTAL EPIPHARYNGEAL = 1
placeDistance GLOTTAL EPIGLOTTAL = 2
placeDistance (COARTICULATED p1 p2) (COARTICULATED p3 p4) = round (realToFrac (placeDistance p1 p3 + placeDistance p2 p4)/2)
placeDistance x y | x == y = 0
                  | otherwise = 5

phonationDistance :: Phonation -> Phonation -> Int
phonationDistance VOICELESS BREATHY = 1
phonationDistance VOICELESS SLACK = 2
phonationDistance VOICELESS MODAL = 5
phonationDistance VOICELESS STIFF = 4
phonationDistance VOICELESS CREAKY = 3
phonationDistance BREATHY SLACK = 1
phonationDistance BREATHY MODAL = 2
phonationDistance BREATHY STIFF = 4
phonationDistance BREATHY CREAKY = 4
phonationDistance SLACK MODAL = 1
phonationDistance SLACK STIFF = 3
phonationDistance SLACK CREAKY = 3
phonationDistance MODAL STIFF = 1
phonationDistance MODAL CREAKY = 2
phonationDistance STIFF CREAKY = 1
phonationDistance ASPIRATED VOICELESS = 2
phonationDistance ASPIRATED BREATHY = 3
phonationDistance ASPIRATED SLACK = 4
phonationDistance ASPIRATED MODAL = 5
phonationDistance ASPIRATED STIFF = 5
phonationDistance ASPIRATED CREAKY = 5
phonationDistance BREATHY VOICELESS = 1
phonationDistance SLACK VOICELESS = 2
phonationDistance MODAL VOICELESS = 5
phonationDistance STIFF VOICELESS = 4
phonationDistance CREAKY VOICELESS = 3
phonationDistance SLACK BREATHY = 1
phonationDistance MODAL BREATHY = 2
phonationDistance STIFF BREATHY = 4
phonationDistance CREAKY BREATHY = 4
phonationDistance MODAL SLACK = 1
phonationDistance STIFF SLACK = 3
phonationDistance CREAKY SLACK = 3
phonationDistance STIFF MODAL = 1
phonationDistance CREAKY MODAL = 2
phonationDistance CREAKY STIFF = 1
phonationDistance VOICELESS ASPIRATED = 2
phonationDistance BREATHY ASPIRATED = 3
phonationDistance SLACK ASPIRATED = 4
phonationDistance MODAL ASPIRATED = 5
phonationDistance STIFF ASPIRATED = 5
phonationDistance CREAKY ASPIRATED = 5
phonationDistance x y | x == y = 0
                      | otherwise = 5


airstreamDistance :: Airstream -> Airstream -> Int
airstreamDistance PULMONIC EJECTIVE = 1
airstreamDistance EJECTIVE PULMONIC = 1
airstreamDistance EJECTIVE IMPLOSIVE = 1
airstreamDistance IMPLOSIVE EJECTIVE = 1
airstreamDistance IMPLOSIVE LINGUAL = 3
airstreamDistance LINGUAL IMPLOSIVE = 3
airstreamDistance x y | x == y = 0
                      | otherwise = 5
