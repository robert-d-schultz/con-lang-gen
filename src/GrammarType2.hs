module GrammarType2
( Manifest(..)
, GrammarSystem(..)
, Gender(..)
, Animacy(..)
, Case(..)
, Number(..)
, Honorific(..)
, Definiteness(..)
, Specificity(..)
, ParticleSystem(..)
, ExponentSystem(..)
) where

import Prelude
import PhonemeType2
import OtherData2

-- How does the grammatical category manifest?
-- NoManifest - there is no concept of this in the language
-- Article/Particle - category uses particles to manifest (the, an, a for definiteness/specificity in English)
-- Exponent - manifests by changing the word
  -- Identity - no change, different from NoManifest tho
  -- Affixation
    -- Prefix
    -- Suffix
    -- Infix
    -- Circumfix
  -- Reduplication - duplicate part of the word
  -- Internal modification - just change something, use phonological rule maybe, although straight [Morpheme] -> [Morpheme] might be better
  -- Subtraction - same thing as affixation, really

data Manifest a = NoManifest | Particle a | Exponent a deriving (Eq, Show, Read)

-- Grammar system data
data GrammarSystem = GrammarSystem
    { gSys :: Manifest ([Gender], Int)
    , aSys :: Manifest ([Animacy], Int)
    , cSys :: Manifest ([Case], Int)
    , nSys :: Manifest ([Number], Int)
    , hSys :: Manifest ([Honorific], Int)
    , dSys :: Manifest ([Definiteness], Int)
    , sSys :: Manifest ([Specificity], Int)
    } deriving (Show)

data Gender       = M | F | COM | N  deriving (Eq, Show, Read)
data Animacy      = AN | HUM | NHUM | ZO | INAN deriving (Eq, Show, Read)
data Case         = NOM | ACC | ERG | GEN | DAT | LOC | PREP | ABL | INS | VOC deriving (Eq, Show, Read)
data Number       = SG | DU | TRI | PA | PL deriving (Eq, Show, Read)
data Honorific    = FAM | FORM deriving (Eq, Show, Read)
data Definiteness = DEF | INDF deriving (Eq, Show, Read)
data Specificity  = SPEC | NSPEC deriving (Eq, Show, Read)
-- data Topic = TOP | NTOP deriving (Eq, Show, Read)

-- Particle system
data ParticleSystem = ParticleSystem [(Morpheme, (Manifest Gender, Manifest Animacy, Manifest Case, Manifest Number, Manifest Honorific, Manifest Definiteness, Manifest Specificity))] deriving (Eq, Show, Read)
-- Exponent system
data ExponentSystem = ExponentSystem [(Morpheme, (Manifest Gender, Manifest Animacy, Manifest Case, Manifest Number, Manifest Honorific, Manifest Definiteness, Manifest Specificity))] deriving (Eq, Show, Read)

--data ParticleType = PRE | POST
--data ExponentType = PREFIX | SUFFIX | CIRCUMFIX deriving (Show, Read)

-- Conjugation stuff
data Tense = PST | REM | REC | NPST | PRS | NFUT | FUT | IMMF | REMF deriving (Eq, Show, Read)
data Aspect = PFV | IPFV deriving (Eq, Show, Read)
data Mood = IND | IRR | DEO | IMP | JUS | OPT | EPIS | SBJV | POT | COND deriving (Eq, Show, Read)
data Person = FIRST | SECOND | THIRD deriving (Eq, Show, Read)
data Voice = ACT | MID | PAS deriving (Eq, Show, Read)
data Evidentiality = EXP | SENS | VIS | NVIS | AUD | INFER | REP | HSY | QUO | ASS deriving (Eq, Show, Read)
data Transitivity = INTR | TR | DITR deriving (Eq, Show, Read)
data Polarity = AFF | NEG deriving (Eq, Show, Read)
data Volition = VOL | NVOL deriving (Eq, Show, Read)
