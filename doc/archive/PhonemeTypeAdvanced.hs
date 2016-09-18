-- Data types for phonemes
module PhonemeTypeAdvanced
( MaybeContour(..)
, MaybeImpossible(..)
, Consonant(..)
, Place (..)
, ActiveArticulator(..)
, PassiveArticulator(..)
, Manner(..)
, Stricture(..)
, Trill(..)
, Length(..)
, Laterality(..)
, Silibance(..)
, AirEscape(..)
, VOT(..)
, Airstream(..)
, Initiator(..)
, Direction(..)
, Phonation(..)
, Vowel(..)
, Height(..)
, Backness(..)
, Roundedness(..)
, ConsonantPhonemeInventory(..)
, VowelPhonemeInventory(..)
) where

import Prelude

data MaybeContour a b = NoContour b | Contour a b deriving (Eq, Show, Read)
data MaybeImpossible a = Possible a | Impossible deriving (Eq, Show, Read)

data Consonant =  Consonant
    { place :: MaybeContour Place Place
    , manner :: MaybeContour Manner Manner
    , airstream :: MaybeContour Airstream Airstream
    , phonation :: MaybeContour Phonation Phonation
    } deriving (Eq, Show, Read)

data Place = Place
    { active :: ActiveArticulator
    , passive :: PassiveArticulator
    } deriving (Eq, Show, Read)

data ActiveArticulator = LOWERLIP | TONGUEBLADE | TONGUETIP | TONGUEUNDER | TONGUEBODY | TONGUEROOT | LARYNX  deriving (Eq, Show, Read, Enum, Bounded)

data PassiveArticulator = UPPERLIP | UPPERTEETH | TEETHRIDGE | RIDGE | BACKRIDGE | HARDPALATE | SOFTPALATE | UVULA | PHARYNX | EPIGLOTTIS | GLOTTIS deriving (Eq, Show, Read, Enum, Bounded)

data Manner = Manner
    { stricture :: Stricture
    , trill :: MaybeImpossible Trill
    , mannerLength :: Length
    , laterality :: MaybeImpossible Laterality
    , silibance :: MaybeImpossible Silibance
    , airescape :: AirEscape
    , vot :: MaybeImpossible VOT
    , aspiration :: MaybeImpossible Aspiration
--    , release :: Release
    } deriving (Eq, Show, Read)

data Stricture = OCCLUSION | TURBULENT | SLIGHTTURBULENT deriving (Eq, Show, Read, Enum, Bounded)

data Trill = TRILLED | NOTTRILLED deriving (Eq, Show, Read, Enum, Bounded)

data Length = SHORT | NORMAL | LONG deriving (Eq, Show, Read, Enum, Bounded)

data Laterality = LATERAL | NONLATERAL deriving (Eq, Show, Read, Enum, Bounded)

data Silibance = SILIBANT | NONSILIBANT deriving (Eq, Show, Read, Enum, Bounded)

data AirEscape = NASALIZED | ORAL deriving (Eq, Show, Read, Enum, Bounded)

data VOT = POSITIVE | ZERO | NEGATIVE deriving (Eq, Show, Read, Enum, Bounded)

data Aspiration = ASPIRATE | NOAUDIBLE | LATERALR | NASAL deriving (Eq, Show, Read, Enum, Bounded)

data Airstream = Airstream
    { initiator :: Initiator
    , direction :: Direction
    } deriving (Eq, Show, Read)

data Initiator = LINGUAL | GLOTTIC | PULMONIC deriving (Eq, Show, Read, Enum, Bounded)

data Direction = INGRESSIVE | EGRESSIVE deriving (Eq, Show, Read, Enum, Bounded)

data Phonation = VOICELESS | BREATHY | SLACK | MODAL | STIFF | CREAKY | CLOSURE deriving (Eq, Show, Read, Enum, Bounded)

--Vowel specific
data Vowel = VowelFeaturesContour
    { height :: MaybeContour Height Height
    , backness :: MaybeContour Backness Backness
    , roundedness :: MaybeContour Roundedness Roundedness
    , vowelLength :: Length
    , airescape2 :: AirEscape
    , airstream2 :: Airstream
    , phonation2 :: MaybeContour Phonation Phonation
    } deriving (Eq, Show, Read)

data Height = CLOSE | NEARCLOSE | CLOSEMID | MID | OPENMID | NEAROPEN | OPEN deriving (Eq, Show, Read, Enum, Bounded)

data Backness = BACK | NEARBACK | CENTRAL | NEARFRONT | FRONT deriving (Eq, Show, Read, Enum, Bounded)

data Roundedness = UNROUNDED | ROUNDED deriving (Eq, Show, Read, Enum, Bounded)

data ConsonantPhonemeInventory = ConsonantPhonemeInventory [Consonant] deriving (Eq, Show, Read)
data VowelPhonemeInventory = VowelPhonemeInventory [Vowel] deriving (Eq, Show, Read)
