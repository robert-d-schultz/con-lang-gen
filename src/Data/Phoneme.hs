module Data.Phoneme
( SyllWord(..)
, ConsCluster
, Syllable(..)
, MorphWord(..)
, Morpheme(..)
, Phoneme(..)
, Place (..)
, Manner(..)
, Phonation(..)
, Airstream(..)
, Height(..)
, Backness(..)
, Roundedness(..)
, Length(..)
, Tone(..)
, Stress(..)
) where

import ClassyPrelude hiding (Word)
import Data.List (elemIndex)

-- Used to parse out syllables from a word
newtype SyllWord = SyllWord [Syllable] deriving (Eq, Ord, Read, Show)
type ConsCluster = [Phoneme]
data Syllable = Syllable
              { getOnset :: [Phoneme]
              , getNucleus :: Phoneme
              , getCoda :: [Phoneme]
              , getTone :: Tone
              , getStress :: Stress
              } deriving (Eq, Ord, Read, Show)

-- Word/Morpheme/Phoneme
newtype MorphWord = MorphWord { getMorphemes :: [Morpheme] } deriving (Eq, Ord, Read, Show)
newtype Morpheme = Morpheme { getPhonemes :: [Phoneme] } deriving (Eq, Ord, Read, Show)

data Phoneme = Consonant
           { getPlace :: Place
           , getManner :: Manner
           , getVoice :: Phonation
           , getAirstream :: Airstream
           }
           | Vowel
           { getHeight :: Height
           , getBackness :: Backness
           , getRoundedness :: Roundedness
           , getLength :: Length
           }
           | Diphthong
           { getHeight1 :: Height
           , getBackness1 :: Backness
           , getRoundedness1 :: Roundedness
           , getHeight2 :: Height
           , getBackness2 :: Backness
           , getRoundedness2 :: Roundedness
           , getLength :: Length
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
            | ALVEOLAR     -- not really used
            | POSTALVEOLAR -- not really used
            | COARTICULATED { getPlaceA :: Place, getPlaceB :: Place}
            deriving (Eq, Ord, Read)

-- Enum for Place (because Coarticulated fucked it all up)
instance Enum Place where
  toEnum n | n < 20 = unsafeIndex [ BILABIAL, LABIODENTAL, LINGUOLABIAL, INTERDENTAL, DENTAL, DENTIALVEOLAR, LAMINALALVEOLAR
                                  , APICOALVEOLAR, PALATOALVEOLAR, APICALRETROFLEX, RETROFLEX, ALVEOLOPALATAL, PALATAL, VELAR
                                  , UVULAR, PHARYNGEAL, EPIPHARYNGEAL, EPIGLOTTAL, GLOTTAL ] n
           | otherwise = error "Prelude.Enum.Place.toEnum: bad argument"

  fromEnum (COARTICULATED a b) = 19 + 19 * fromEnum a + fromEnum b
  fromEnum x = fromMaybe (error "Prelude.Enum.Place.toEnum: bad argument") (elemIndex x [ BILABIAL, LABIODENTAL, LINGUOLABIAL, INTERDENTAL, DENTAL, DENTIALVEOLAR, LAMINALALVEOLAR
                                                                                        , APICOALVEOLAR, PALATOALVEOLAR, APICALRETROFLEX, RETROFLEX, ALVEOLOPALATAL, PALATAL, VELAR
                                                                                        , UVULAR, PHARYNGEAL, EPIPHARYNGEAL, EPIGLOTTAL, GLOTTAL ])

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
            | LFLAP
            | CLICK
             deriving (Eq, Ord, Read, Enum, Bounded)

-- Phonation
data Phonation  = VOICELESS
                | BREATHY
                | SLACK
                | MODAL
                | STIFF
                | CREAKY
                | ASPIRATED deriving (Eq, Ord, Read, Enum, Bounded)


data Airstream = PULMONIC  -- pulmonic egressive
               | EJECTIVE  -- glottalic egressive
               | IMPLOSIVE -- glottalic ingressive
               | LINGUAL   -- lingual ingressive (clicks)
               deriving (Eq, Ord, Read, Enum, Bounded)

-- Vowel stuff
data Height      = CLOSE
                 | NEARCLOSE
                 | CLOSEMID
                 | MID
                 | OPENMID
                 | NEAROPEN
                 | OPEN deriving (Eq, Ord, Read, Enum, Bounded)

data Backness    = FRONT
                 | NEARFRONT
                 | CENTRAL
                 | NEARBACK
                 | BACK deriving (Eq, Ord, Read, Enum, Bounded)

data Roundedness = ROUNDED
                 | UNROUNDED deriving (Eq, Ord, Read, Enum, Bounded)

data Length = SHORT
            | NORMAL
            | LONG deriving (Eq, Ord, Read, Enum, Bounded)

-- Tone
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


data Stress = NONES
            | PRIMARYS
            | SECONDARYS deriving (Eq, Ord, Read, Enum, Bounded)

-- show instances
instance Show Phoneme where
  -- Don't show phonation for ejectives
  show (Consonant p m _ EJECTIVE) = show p ++ " " ++ show m ++ " " ++ " Ejective"
  -- Don't show manner (always STOP) for implosives
  show (Consonant p _ h IMPLOSIVE) = show h ++ " " ++ show p ++ " " ++ " Implosive"
  -- Don't show manner (always CLICK) for clicks
  show (Consonant p _ h LINGUAL) = show h ++ " " ++ show p ++ " " ++ " Click"
  -- Don't show airstream for everything else
  show (Consonant p m h _) = show h ++ " " ++ show p ++ " " ++ show m ++ " "
  show (Vowel h b r l) = show l ++ " " ++ show h ++ " " ++ show b ++ " " ++ show r ++ " Vowel"
  show (Diphthong h1 b1 r1 h2 b2 r2 l) = show l ++ h ++ b ++ r where
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
                     COARTICULATED a b -> show a ++ "–" ++ show b

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
                     CLICK        -> "Click"


instance Show Phonation where
  show h = case h of VOICELESS -> "Voiceless"
                     BREATHY   -> "Breathy"
                     SLACK     -> "Slack"
                     MODAL     -> "Modal"
                     STIFF     -> "Stiff"
                     CREAKY    -> "Creaky"
                     ASPIRATED -> "Aspirated"

instance Show Airstream where
  show a = case a of PULMONIC  -> "Pulmonic"
                     EJECTIVE  -> "Ejective"
                     IMPLOSIVE -> "Implosive"
                     LINGUAL   -> "Click"


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

instance Show Stress where
 show s = case s of NONES      -> "No stress"
                    PRIMARYS   -> "Primary stress"
                    SECONDARYS -> "Secondary stress"
