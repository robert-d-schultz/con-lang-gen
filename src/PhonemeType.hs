-- Data types for phonemes
module PhonemeType
( MaybeImpossible(..)
, MaybeBlank(..)
, MaybeUnspecified(..)
, ConsonantFeaturesContour(..)
, ConsonantFeatures(..)
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
, VowelFeaturesContour(..)
, VowelFeatures(..)
, Height(..)
, Backness(..)
, Roundedness(..)
, PhonemeInventory(..)
) where

import Prelude

data MaybeImpossible a = Possible a | Impossible deriving (Eq, Show, Read)
data MaybeBlank a = Filled a | Blank deriving (Eq, Show, Read)
data MaybeUnspecified a = Specified a | Unspecified deriving (Eq, Show, Read)

data ConsonantFeaturesContour = ConsonantFeaturesContour
    { firstCContour :: ConsonantFeatures
    , secondCContour :: MaybeBlank (MaybeUnspecified ConsonantFeatures)
    } deriving (Eq, Show, Read)

data ConsonantFeatures = ConsonantFeatures
    { place :: MaybeUnspecified Place
    , manner :: MaybeUnspecified Manner
    , airstream :: MaybeUnspecified Airstream
    , phonation :: MaybeUnspecified Phonation
    } deriving (Eq, Show, Read)

data Place = Place
    { active :: MaybeUnspecified ActiveArticulator
    , passive :: MaybeUnspecified PassiveArticulator
    } deriving (Eq, Show, Read)

data ActiveArticulator = LOWERLIP | TONGUEBLADE | TONGUETIP | TONGUEUNDER | TONGUEBODY | TONGUEROOT | LARYNX  deriving (Eq, Show, Read, Enum, Bounded)

data PassiveArticulator = UPPERLIP | UPPERTEETH | TEETHRIDGE | RIDGE | BACKRIDGE | HARDPALATE | SOFTPALATE | UVULA | PHARYNX | EPIGLOTTIS | GLOTTIS deriving (Eq, Show, Read, Enum, Bounded)

data Manner = Manner
    { stricture :: MaybeUnspecified Stricture
    , trill :: MaybeImpossible (MaybeUnspecified Trill)
    , length :: MaybeUnspecified Length
    , laterality :: MaybeImpossible (MaybeUnspecified Laterality)
    , silibance :: MaybeImpossible (MaybeUnspecified Silibance)
    , airescape :: MaybeUnspecified AirEscape
    , vot :: MaybeImpossible (MaybeUnspecified VOT)
--    , release :: Release
    } deriving (Eq, Show, Read)

data Stricture = OCCLUSION | TURBULENT | SLIGHTTURBULENT deriving (Eq, Show, Read, Enum, Bounded)

data Trill = TRILLED | NOTTRILLED deriving (Eq, Show, Read, Enum, Bounded)

data Length = SHORT | NORMAL | LONG deriving (Eq, Show, Read, Enum, Bounded)

data Laterality = LATERAL | NONLATERAL deriving (Eq, Show, Read, Enum, Bounded)

data Silibance = SILIBANT | NONSILIBANT deriving (Eq, Show, Read, Enum, Bounded)

data AirEscape = NASALIZED | ORAL deriving (Eq, Show, Read, Enum, Bounded)

data VOT = POSITIVE | ZERO | NEGATIVE deriving (Eq, Show, Read, Enum, Bounded)

--data Release = NOAUDIBLE | LATERAL | NASAL deriving (Eq, Show, Read, Enum, Bounded)

data Airstream = Airstream
    { initiator :: MaybeUnspecified Initiator
    , direction :: MaybeUnspecified Direction
    } deriving (Eq, Show, Read)

data Initiator = LINGUAL | GLOTTIC | PULMONIC deriving (Eq, Show, Read, Enum, Bounded)

data Direction = INGRESSIVE | EGRESSIVE deriving (Eq, Show, Read, Enum, Bounded)

data Phonation = VOICELESS | BREATHY | SLACK | MODAL | STIFF | CREAKY | CLOSURE deriving (Eq, Show, Read, Enum, Bounded)


data VowelFeaturesContour = VowelFeaturesContour
    { firstVContour :: VowelFeatures
    , secondVContour :: MaybeBlank (MaybeUnspecified VowelFeatures)
    } deriving (Eq, Show, Read)

data VowelFeatures = VowelFeatures
    { height :: MaybeUnspecified Height
    , backness :: MaybeUnspecified Backness
    , roundedness :: MaybeUnspecified Roundedness
    , length2 :: MaybeUnspecified Length
    , airescape2 :: MaybeUnspecified AirEscape
    , airstream2 :: MaybeUnspecified Airstream
    , phonation2 :: MaybeUnspecified Phonation
    } deriving (Eq, Show, Read)

data Height = CLOSE | NEARCLOSE | CLOSEMID | MID | OPENMID | NEAROPEN | OPEN deriving (Eq, Show, Read, Enum, Bounded)

data Backness = BACK | NEARBACK | CENTRAL | NEARFRONT | FRONT deriving (Eq, Show, Read, Enum, Bounded)

data Roundedness = UNROUNDED | ROUNDED deriving (Eq, Show, Read, Enum, Bounded)

data PhonemeInventory = PhonemeInventory [ConsonantFeaturesContour] deriving (Eq, Show, Read)
