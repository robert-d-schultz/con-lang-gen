-- Data types for phonemes
module PhonemeType2
( Phone(..)
, Place (..)
, Manner(..)
, Voice(..)
, Height(..)
, Backness(..)
, Roundedness(..)
, PhonemeInventory(..)
) where

import Prelude

data Phone = Consonant
           { place :: Place
           , manner :: Manner
           , voice :: Voice
           , csymbol :: String
           }
           | Vowel
           { height :: Height
           , backness :: Backness
           , roundedness :: Roundedness
           , vsymbol :: String
           }
           | Blank deriving (Eq, Ord, Show, Read)

data Place  = BILABIAL
            | LABIODENTAL
            | DENTAL
            | ALVEOLAR
            | PALATOALVEOLAR
            | RETROFLEX
            | ALVEOLOPALATAL
            | PALATAL
            | VELAR
            | UVULAR
            | PHARYNGEAL
            | GLOTTAL deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Manner = NASAL
            | STOP
            | AFFRICATE
            | FRICATIVE
            | APPROXIMANT
            | FLAP
            | TRILL
            | LAFFRICATE
            | LFRICATIVE
            | LAPPROXIMANT
            | LFLAP deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Voice  = VOICELESS
            | VOICED deriving (Eq, Ord, Show, Read, Enum, Bounded)

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

data PhonemeInventory = PhonemeInventory [Phone] deriving (Eq, Ord, Show, Read)
