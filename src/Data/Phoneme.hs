{-# LANGUAGE NoImplicitPrelude #-}
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

import ClassyPrelude hiding (Word)

newtype Word = Word { getMorphemes :: [Morpheme] } deriving (Eq, Ord, Show, Read)
newtype Morpheme = Morpheme { getPhonemes :: [Phoneme] } deriving (Eq, Ord, Show, Read)

data Phoneme = Consonant
           { cplace :: Place
           , cmanner :: Manner
           , cvoice :: Phonation
           }
           | Vowel
           { vheight :: Height
           , vbackness :: Backness
           , vroundedness :: Roundedness
           , vlength :: Length
           , vtone :: Tone
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
            | GLOTTAL deriving (Eq, Ord, Read, Enum, Bounded)

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
            | LFLAP deriving (Eq, Ord, Read, Enum, Bounded)

-- Phonation
data Phonation  = VOICELESS
                | BREATHY
                | SLACK
                | MODAL
                | STIFF
                | CREAKY
                | ASPIRATED deriving (Eq, Ord, Read, Enum, Bounded)

-- Vowel stuff
data Height      = CLOSE
                 | NEARCLOSE
                 | CLOSEMID
                 | MID
                 | OPENMID
                 | NEAROPEN
                 | OPEN deriving (Eq, Ord, Read, Enum, Bounded)

data Backness    = BACK
                 | NEARBACK
                 | CENTRAL
                 | NEARFRONT
                 | FRONT deriving (Eq, Ord, Read, Enum, Bounded)

data Roundedness = ROUNDED
                 | UNROUNDED deriving (Eq, Ord, Read, Enum, Bounded)

data Length = SHORT
            | NORMAL
            | LONG deriving (Eq, Ord, Read, Enum, Bounded)

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
          | PEAKT deriving (Eq, Ord, Read, Enum, Bounded)

-- show instances
instance Show Place where
  show p = case p of LABIAL         -> "Labial"
                     BILABIAL       -> "Bilabial"
                     LABIODENTAL    -> "Labiodental"
                     CORONAL        -> "Coronal"
                     DENTIALVEOLAR  -> "Denti-alveolar"
                     DENTAL         -> "Dental"
                     ALVEOLAR       -> "Alveolar"
                     POSTALVEOLAR   -> "Post-Alveolar"
                     RETROFLEX      -> "Retroflex"
                     DORSAL         -> "Dorsal"
                     ALVEOLOPALATAL -> "Alveolo-palatal"
                     PALATAL        -> "Palatal"
                     VELAR          -> "Velar"
                     UVULAR         -> "Uvular"
                     LARYNGEAL      -> "Laryngeal"
                     EPIPHARYNGEAL  -> "Epipharyngeal"
                     PHARYNGEAL     -> "Pharyngeal"
                     EPIGLOTTAL     -> "Epiglottal"
                     GLOTTAL        -> "Glottal"

instance Show Manner where
  show m = case m of NASAL        -> "Nasal"
                     STOP         -> "Stop"
                     SAFFRICATE   -> "Silibant affricate"
                     AFFRICATE    -> "Affricate"
                     SILIBANT     -> "Silibant fricative"
                     FRICATIVE    -> "Fricative"
                     APPROXIMANT  -> "Approximant"
                     FLAP         -> "Flap"
                     TRILL        -> "Trill"
                     LAFFRICATE   -> "Lateral affricate"
                     LFRICATIVE   -> "Lateral fricative"
                     LAPPROXIMANT -> "Lateral approximant"
                     LFLAP        -> "Lateral flap"

instance Show Phonation where
  show h = case h of VOICELESS -> "Voiceless"
                     BREATHY   -> "Breathy"
                     SLACK     -> "Slack"
                     MODAL     -> "Modal"
                     STIFF     -> "Stiff"
                     CREAKY    -> "Creaky"
                     ASPIRATED -> "Aspirated"

instance Show Height where
  show h = case h of CLOSE     -> "Close"
                     NEARCLOSE -> "Near-close"
                     CLOSEMID  -> "Close-mid"
                     MID       -> "Mid"
                     OPENMID   -> "Open-mid"
                     NEAROPEN  -> "Near-open"
                     OPEN      -> "Open"

instance Show Backness where
  show b = case b of BACK      -> "Back"
                     NEARBACK  -> "Near-back"
                     CENTRAL   -> "Central"
                     NEARFRONT -> "Near-front"
                     FRONT     -> "Front"

instance Show Roundedness where
  show r = case r of ROUNDED   -> "Rounded"
                     UNROUNDED -> "Unrounded"

instance Show Length where
  show l = case l of SHORT  -> "Short"
                     NORMAL -> ""
                     LONG   -> "Long"

instance Show Tone where
  show t = case t of NONET   -> ""
                     TOPT    -> "Top"
                     HIGHT   -> "High"
                     MIDT    -> "Mid"
                     LOWT    -> "Low"
                     BOTTOMT -> "Bottom"
                     FALLT   -> "Falling"
                     HFALLT  -> "High falling"
                     LFALLT  -> "Low falling"
                     RISET   -> "Rising"
                     HRISET  -> "High rising"
                     LRISET  -> "Low rising"
                     DIPT    -> "Dipping"
                     PEAKT   -> "Peaking"
