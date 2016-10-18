-- Data types for phonemes
module PhonemeType2
( Phoneme(..)
, Place (..)
, Manner(..)
, Phonation(..)
, Height(..)
, Backness(..)
, Roundedness(..)
, PhonemeInventory(..)
) where

import Prelude

data Phoneme = Consonant
           { place :: Place
           , manner :: Manner
           , voice :: Phonation
           , csymbol :: String
           }
           | Vowel
           { height :: Height
           , backness :: Backness
           , roundedness :: Roundedness
           , vsymbol :: String
           }
           | Diphthong
           { height1 :: Height
           , backness1 :: Backness
           , roundedness1 :: Roundedness
           , vsymbol1 :: String
           , height2 :: Height
           , backness2 :: Backness
           , roundedness2 :: Roundedness
           , vsymbol2 :: String
           }
           | Blank deriving (Eq, Ord, Show, Read)


-- Place of articulation
data Place  = LABIAL
            | BILABIAL
            | LABIODENTAL
            | CORONAL
            | DENTIALVEOLAR
            | DENTAL
            | ALVEOLAR
            | POSTALVEOLAR
            | RETROFLEX
            | DORSAL
            | ALVEOLOPALATAL
            | PALATAL
            | VELAR
            | UVULAR
            | LARYNGEAL
            | EPIPHARYNGEAL
            | PHARYNGEAL
            | EPIGLOTTAL
            | GLOTTAL deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- Manner
data Manner = NASAL
            | STOP
            | SAFFRICATE
            | AFFRICATE
            | SILIBANT
            | FRICATIVE
            | APPROXIMANT
            | FLAP
            | TRILL
            | LAFFRICATE
            | LFRICATIVE
            | LAPPROXIMANT
            | LFLAP deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- Phonation
data Phonation  = VOICELESS
                | BREATHY
                | SLACK
                | MODAL
                | STIFF
                | CREAKY
                | ASPIRATED deriving (Eq, Ord, Show, Read, Enum, Bounded)



-- Vowel stuff
data Height      = CLOSE
                 | NEARCLOSE
                 | CLOSEMID
                 | MID
                 | OPENMID
                 | NEAROPEN
                 | OPEN deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Backness    = BACK
                 | NEARBACK
                 | CENTRAL
                 | NEARFRONT
                 | FRONT deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Roundedness = ROUNDED
                 | UNROUNDED deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PhonemeInventory = PhonemeInventory [Phoneme] deriving (Eq, Ord, Show, Read)
