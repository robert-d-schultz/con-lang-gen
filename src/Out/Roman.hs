module Out.Roman
( romanizeWord
, romanizeMorpheme
, romanizePhoneme
) where

import Prelude hiding (Word)

import Data.Phoneme

romanizeWord :: Word -> String
romanizeWord (Word morphemes) = concatMap romanizeMorpheme morphemes

romanizeMorpheme :: Morpheme -> String
romanizeMorpheme (Morpheme phonemes) = romanizePhonemes phonemes

romanizePhonemes :: [Phoneme] -> String
romanizePhonemes [] = ""
romanizePhonemes phonemes
  | length phonemes > 1 && isVowel (head phonemes) && isVowel (head $ tail phonemes) = romanizePhoneme (head phonemes) ++ "\'" ++ romanizePhonemes (tail phonemes)
  | otherwise = romanizePhoneme (head phonemes) ++ romanizePhonemes (tail phonemes)

isVowel :: Phoneme -> Bool
isVowel Vowel{} = True
isVowel _ = False

-- these rules suck, need diacritics and a pool to pull from as needed
romanizePhoneme :: Phoneme -> String
romanizePhoneme (Consonant p m v)
  | v == ASPIRATED = romanizePhoneme (Consonant p m MODAL) ++ "h"
  | v == BREATHY = romanizePhoneme (Consonant p m VOICELESS)
  | v == SLACK = romanizePhoneme (Consonant p m VOICELESS)
  | v == STIFF = romanizePhoneme (Consonant p m MODAL)
  | v == CREAKY = romanizePhoneme (Consonant p m MODAL)
  | p `elem` [LABIAL, BILABIAL] && m == NASAL && v == MODAL = "m"
  | p `elem`[CORONAL, DENTIALVEOLAR, ALVEOLAR] && m == NASAL && v == MODAL = "n"
  | p `elem` [DORSAL, VELAR] && m == NASAL && v == MODAL = "ng"
  | p `elem` [LABIAL, BILABIAL, LABIODENTAL] && m == STOP && v == VOICELESS = "p"
  | p `elem` [LABIAL, BILABIAL, LABIODENTAL] && m == STOP && v == MODAL = "b"
  | p `elem`[CORONAL, DENTIALVEOLAR, ALVEOLAR] && m == STOP && v == VOICELESS = "t"
  | p `elem`[CORONAL, DENTIALVEOLAR, ALVEOLAR] && m == STOP && v == MODAL = "d"
  | p `elem` [DORSAL, VELAR] && m == STOP && v == VOICELESS = "k"
  | p `elem` [DORSAL, VELAR] && m == STOP && v == MODAL = "g"
  | p `elem` [LABIAL, LABIODENTAL] && m `elem`  [FRICATIVE, SILIBANT] && v == VOICELESS = "f"
  | p `elem` [LABIAL, LABIODENTAL] && m `elem`  [FRICATIVE, SILIBANT] && v == MODAL = "v"
  | p == DENTAL && m `elem`  [FRICATIVE, SILIBANT] && v `elem` [MODAL, VOICELESS] = "th"
  | p `elem`[CORONAL, DENTIALVEOLAR, ALVEOLAR] && m `elem` [FRICATIVE, SILIBANT] && v == VOICELESS = "s"
  | p `elem`[CORONAL, DENTIALVEOLAR, ALVEOLAR] && m `elem` [FRICATIVE, SILIBANT] && v == MODAL = "z"
  | p == POSTALVEOLAR && m `elem` [FRICATIVE, SILIBANT] && v == VOICELESS = "sh"
  | p == POSTALVEOLAR && m `elem` [FRICATIVE, SILIBANT] && v == MODAL = "zh"
  | p `elem` [LARYNGEAL, GLOTTAL] && m == FRICATIVE && v == VOICELESS = "h"
  | p `elem`[CORONAL, DENTIALVEOLAR, ALVEOLAR] && m `elem` [SAFFRICATE, AFFRICATE] && v == VOICELESS = "ts"
  | p == POSTALVEOLAR && m `elem` [SAFFRICATE, AFFRICATE] && v == VOICELESS = "ch"
  | p == POSTALVEOLAR && m `elem` [SAFFRICATE, AFFRICATE] && v == MODAL = "j"
  | p `elem`[CORONAL, DENTIALVEOLAR, ALVEOLAR] && m `elem` [APPROXIMANT, TRILL] && v == MODAL = "r"
  | p `elem`[CORONAL, DENTIALVEOLAR, ALVEOLAR] && m == LAPPROXIMANT && v == MODAL = "l"
  | p `elem` [ALVEOLOPALATAL, PALATAL] && m == APPROXIMANT = "y"
  | p == VELAR && m == APPROXIMANT = "w"
  | ((p <= DORSAL || p == UVULAR) && m == APPROXIMANT) || ((p <= DORSAL || p == UVULAR) && m `elem` [APPROXIMANT, TRILL, FLAP] ) = "r"
  | m `elem` [LAPPROXIMANT, LFRICATIVE, LAFFRICATE, LFLAP] = "l"
  | p `elem` [UVULAR, LARYNGEAL, PHARYNGEAL, EPIPHARYNGEAL, EPIGLOTTAL] && m == STOP = "g"
  | p `elem` [LARYNGEAL, PHARYNGEAL, EPIPHARYNGEAL, EPIGLOTTAL, GLOTTAL] = "h"
  | m == NASAL = "n"
  | p `elem` [DORSAL, VELAR] && m == STOP = "k"
  | m == AFFRICATE = romanizePhoneme (Consonant p STOP v) ++ romanizePhoneme (Consonant p FRICATIVE v)
  | m == SAFFRICATE = romanizePhoneme (Consonant p STOP v) ++ romanizePhoneme (Consonant p SILIBANT v)
  | p == PALATAL = romanizePhoneme (Consonant VELAR m v)
  | p == VELAR = romanizePhoneme (Consonant UVULAR m v)
  | p == RETROFLEX = romanizePhoneme (Consonant ALVEOLAR m v)
  | p == DENTAL = romanizePhoneme (Consonant ALVEOLAR m v)
  | otherwise = "h"

romanizePhoneme (Vowel h b r l t)
  | h == NEAROPEN && b == FRONT && r == UNROUNDED = "a"
  | h == CLOSEMID && b == FRONT && r == UNROUNDED = "e"
  | h == CLOSE && b == FRONT && r == UNROUNDED = "e"
  | h == NEARCLOSE && b == NEARFRONT && r == UNROUNDED = "i"
  | h == OPEN && b == BACK && r == ROUNDED = "o"
  | h == CLOSE && b == BACK && r == ROUNDED && l == LONG = "o"
  | h == OPENMID && b == BACK && r == ROUNDED = "u"
  | l == LONG = concat $ replicate 2 (romanizePhoneme (Vowel h b r NORMAL t))
  | l == SHORT = romanizePhoneme (Vowel h b r NORMAL t)
  | r `elem` [UNROUNDED, ROUNDED] = romanizePhoneme (Vowel h b DEFAULT l t)
  | h `elem` [NEARCLOSE, CLOSEMID, MID, OPENMID] && b == BACK = "o"
  | h `elem` [CLOSEMID, MID, OPENMID] && b == FRONT = "e"
  | h `elem` [OPENMID, NEAROPEN, OPEN] = "a"
  | h `elem` [CLOSE, NEARCLOSE] && b `elem` [FRONT, NEARFRONT] = "i"
  | h `elem` [CLOSE, NEARCLOSE] && b `elem` [BACK, NEARBACK, CENTRAL] = "o"
  | h `elem` [CLOSEMID, MID, OPENMID] && b `elem` [CENTRAL, NEARFRONT, NEARBACK] = "u"
  | otherwise = "u"

romanizePhoneme (Diphthong h b r h2 b2 r2 l t) = romanizePhoneme (Vowel h b r l t) ++ romanizePhoneme (Vowel h2 b2 r2 l t)
