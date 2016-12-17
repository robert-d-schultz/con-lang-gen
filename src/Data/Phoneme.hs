-- Data types for phonemes
module Data.Phoneme
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
, Tone(..)
) where

import Prelude hiding (Word)

data Word = Word { getMorphemes :: [Morpheme] } deriving (Eq, Ord, Show, Read)
data Morpheme = Morpheme { getPhonemes :: [Phoneme] } deriving (Eq, Ord, Show, Read)

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
           , vtone :: Tone
           , vsymbol :: String
           }
           | Diphthong
           { dheight1 :: Height
           , dbackness1 :: Backness
           , droundedness1 :: Roundedness
           , dheight2 :: Height
           , dbackness2 :: Backness
           , droundedness2 :: Roundedness
           , dlength :: Length
           , dtone :: Tone
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

data Tone = NONET
          | TOPT
          | HIGHT
          | MIDT
          | LOWT
          | BOTTOMT
          | FALLT
          | HFALLT
          | LFALLT
          | RISET
          | HRISET
          | LRISET
          | DIPT
          | PEAKT deriving (Eq, Ord, Show, Read, Enum, Bounded)
