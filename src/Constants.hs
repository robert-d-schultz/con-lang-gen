{-# LANGUAGE NoImplicitPrelude #-}
module Constants
( impConsonants
) where

import ClassyPrelude

import Data.Phoneme

-- Impossible consonants filter
impConsonants :: Phoneme -> Bool
impConsonants (Consonant p m h)
  | p `notElem` [CORONAL, DENTIALVEOLAR, DENTAL, ALVEOLAR, POSTALVEOLAR, RETROFLEX] && m `elem` [SILIBANT, SAFFRICATE] = True
  | p `elem` [LABIAL, BILABIAL, LABIODENTAL, LARYNGEAL, EPIPHARYNGEAL, PHARYNGEAL, EPIGLOTTAL, GLOTTAL] && m `elem` [LAFFRICATE, LAPPROXIMANT, LFRICATIVE, LFRICATIVE, LFLAP] = True
  | p `elem` [DORSAL, ALVEOLOPALATAL, PALATAL, VELAR, UVULAR, GLOTTAL] && m `elem` [FLAP, TRILL] = True
  | p == GLOTTAL && m `elem` [STOP, AFFRICATE] && h /= VOICELESS = True
  | m /= STOP && h == ASPIRATED = True
  | p `elem` [LARYNGEAL, EPIPHARYNGEAL, PHARYNGEAL, EPIGLOTTAL, GLOTTAL] && m == NASAL = True
  | otherwise = False
