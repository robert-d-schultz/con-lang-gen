-- Data types for phonemes
module PhonemeType
( MaybeConsonant(..)
, Place (..)
, Manner(..)
, Voice(..)
, Vowel(..)
, Height(..)
, Backness(..)
, Roundedness(..)
, PhonemeInventory(..)
) where

import Prelude

data MaybeConsonant = Consonant
                    { place :: Place
                    , manner :: Manner
                    , voice :: Voice
                    , csymbol :: String
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

data Vowel = Vowel
    { height :: Height
    , backness :: Backness
    , roundedness :: Roundedness
    , vsymbol :: String
    } deriving (Eq, Ord, Show, Read)

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

data PhonemeInventory = PhonemeInventory [MaybeConsonant] [Vowel] deriving (Eq, Ord, Show, Read)
