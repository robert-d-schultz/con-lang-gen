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
           | Blank deriving (Eq, Ord, Read)


-- Place of articulation
data Place  = BILABIAL
            | LABIODENTAL
            | LINGUOLABIAL
            | INTERDENTAL
            | DENTAL
            | DENTIALVEOLAR
            | LAMINALALVEOLAR
            | APICOALVEOLAR
            | PALATOALVEOLAR
            | APICALRETROFLEX
            | RETROFLEX
            | ALVEOLOPALATAL
            | PALATAL
            | VELAR
            | UVULAR
            | PHARYNGEAL
            | EPIPHARYNGEAL
            | EPIGLOTTAL
            | GLOTTAL
            | ALVEOLAR -- not really used
            | POSTALVEOLAR -- not really used
          --  | COARTICULATED Place Place
            deriving (Eq, Ord, Read, Enum, Bounded)

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
instance Show Phoneme where
  show (Consonant p m h) = show h ++ " " ++ show p ++ " " ++ show m
  show (Vowel h b r l t) = show t ++ " " ++ show l ++ " " ++ show h ++ " " ++ show b ++ " " ++ show r ++ " Vowel"
  show (Diphthong h1 b1 r1 h2 b2 r2 l t) = show t ++ show l ++ h ++ b ++ r where
    h | h1 == h2 = show h1
      | otherwise = "(" ++ show h1 ++ " -> " ++ show h2 ++ ")"
    b | b1 == b2 = show b1
      | otherwise = "(" ++ show b1 ++ " -> " ++ show b2 ++ ")"
    r | r1 == r2 = show r1
      | otherwise = "(" ++ show r1 ++ " -> " ++ show r2 ++ ")"
  show Blank = ""

instance Show Place where
  show p = case p of BILABIAL        -> "Bilabial"
                     LABIODENTAL     -> "Labiodental"
                     LINGUOLABIAL    -> "Linguolabial"
                     INTERDENTAL     -> "Interdental"
                     DENTIALVEOLAR   -> "Denti-alveolar"
                     LAMINALALVEOLAR -> "Laminal alveolar"
                     PALATOALVEOLAR  -> "Palato-alveolar"
                     DENTAL          -> "Dental"
                     APICOALVEOLAR   -> "Apico-alveolar"
                     APICALRETROFLEX -> "Apical retroflex"
                     RETROFLEX       -> "Retroflex"
                     ALVEOLOPALATAL  -> "Alveolo-palatal"
                     PALATAL         -> "Palatal"
                     VELAR           -> "Velar"
                     UVULAR          -> "Uvular"
                     PHARYNGEAL      -> "Pharyngeal"
                     EPIPHARYNGEAL   -> "Epiglotto-pharyngeal"
                     EPIGLOTTAL      -> "Epiglottal"
                     GLOTTAL         -> "Glottal"
                     ALVEOLAR        -> "Alveolar" -- not really used
                     POSTALVEOLAR    -> "Postalveolar" -- not really used

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
                     NORMAL -> "'Normal'"
                     LONG   -> "Long"

instance Show Tone where
  show t = case t of NONET   -> "'No tone'"
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
