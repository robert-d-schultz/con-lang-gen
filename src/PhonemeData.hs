-- Data types for phonemes
module PhonemeData
( Word(..)
, Morpheme(..)
, Phoneme(..)
, Place (..)
, Manner(..)
, Phonation(..)
, Height(..)
, Backness(..)
, Roundedness(..)
, Length(..)
) where

import Prelude hiding (Word)

data Word = Word { getMorphemes :: [Morpheme] } deriving (Eq, Show, Read)
data Morpheme = Morpheme { getPhonemes :: [Phoneme] } deriving (Eq, Show, Read)

data Phoneme = Consonant
           { cplace :: Place
           , cmanner :: Manner
           , cvoice :: Phonation
           , csymbol :: String
           }
           | Vowel
           { vheight :: Height
           , vbackness :: Backness
           , vroundedness :: Roundedness
           , vlength :: Length
           , vsymbol :: String
           }
           | Diphthong
           { dheight1 :: Height
           , dbackness1 :: Backness
           , droundedness1 :: Roundedness
           , dlength1 :: Length
           , dheight2 :: Height
           , dbackness2 :: Backness
           , droundedness2 :: Roundedness
           , dlength2 :: Length
           , dsymbol :: String
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

data Roundedness = DEFAULT
                 | ROUNDED
                 | UNROUNDED deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Length = SHORT
            | NORMAL
            | LONG deriving (Eq, Ord, Show, Read, Enum, Bounded)
