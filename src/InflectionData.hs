{-# LANGUAGE StandaloneDeriving #-}
module InflectionData
( Manifest(..)
, Express(..)
, ManifestType(..)
, LexCat(..)
, InflectionSystem(..)
, Gender(..)
, Animacy(..)
, Case(..)
, Number(..)
, Definiteness(..)
, Specificity(..)
, Topic(..)
, Person(..)
, Honorific(..)
, Polarity(..)
, Tense(..)
, Aspect(..)
, Mood(..)
, Voice(..)
, Evidentiality(..)
, Transitivity(..)
, Volition(..)
, ManifestSystem(..)
) where

import Prelude
import PhonemeData
import OtherData

-- Used for 18-tuples
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q) => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) = showTuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, shows i, shows j, shows k, shows l, shows m, shows n, shows o, shows p, shows q]

showTuple :: [ShowS] -> ShowS
showTuple ss = showChar '('
              . foldr1 (\s r -> s . showChar ',' . r) ss
              . showChar ')'

deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o, Eq p, Eq q) => Eq (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)

-- How does the grammatical category manifest?
-- NoManifest - there is no concept of this in the language
-- Particle - category uses particles to manifest (the, an, a for definiteness/specificity in English?)
-- Exponent - manifests by changing the word

data Manifest a = NoManifest | Manifest [(LexCat, ManifestType, Int)] a deriving (Eq, Show, Read)
data Express a  = NoExpress
                | Express { getExp :: a } deriving (Eq, Show, Read)

data ManifestType = Particle | Prefix | Suffix deriving (Eq, Show, Read)

data LexCat = Comp | Infl | Verb | Det | Noun | Adpo | Adj | Adv | Obj | Subj | Pron deriving (Eq, Enum, Show, Read)

-- Inflection system for nouns
data InflectionSystem = InflectionSystem
                      { genSys :: Manifest [Gender]
                      , aniSys :: Manifest [Animacy]
                      , casSys :: Manifest [Case]
                      , numSys :: Manifest [Number]
                      , defSys :: Manifest [Definiteness]
                      , speSys :: Manifest [Specificity]
                      , topSys :: Manifest [Topic]
                      , perSys :: Manifest [Person]
                      , honSys :: Manifest [Honorific]
                      , polSys :: Manifest [Polarity]
                      , tenSys :: Manifest [Tense]
                      , aspSys :: Manifest [Aspect]
                      , mooSys :: Manifest [Mood]
                      , voiSys :: Manifest [Voice]
                      , eviSys :: Manifest [Evidentiality]
                      , traSys :: Manifest [Transitivity]
                      , volSys :: Manifest [Volition]
                      }  deriving (Eq, Show, Read)

-- Grammatical categories
-- For nouns
data Gender        = UGEN | M | F | COM | N  deriving (Eq, Show, Read)
data Animacy       = UANI | AN | HUM | NHUM | ZO | INAN deriving (Eq, Show, Read)
data Case          = UCAS | INTR | ACC | ERG | PEG | INDIR | SEC
                   | NOM | ABS | MTR | DIR | PRIM | ERG2
                   | NOM2 | ABS2 | ABS3 | DTR | OBJ | DRT1
                   | TR
                   | DRT2
                   | OBL1 | OBL2 | OBL3 | OBL4 | OBL5 | OBL6
                   | ADP | PREP | POST
                   | LAT | LOC | ABL
                   | COMP | EQU | IDEN | ABE
                   | DAT | INS | COMIT | INSCOMIT | ORN | BEN
                   | CAUS | DISTR
                   | GEN | POSS | PART
                   | VOC deriving (Eq, Show, Read)
data Number        = UNUM | SG | DU | TRI | PA | PL deriving (Eq, Show, Read)
data Definiteness  = UDEF | DEF | INDF deriving (Eq, Show, Read)
data Specificity   = USPE | SPEC | NSPEC deriving (Eq, Show, Read)
data Topic         = UTOP | TOP | NTOP deriving (Eq, Show, Read)
data Person        = UPER | FIRST | FSTINCL | FSTEXCL | SECOND | THIRD | THRDPROX | THRDOBV deriving (Eq, Show, Read)
-- For nouns and verbs
data Honorific     = UHON | FAM | NEU | FORM deriving (Eq, Show, Read)
data Polarity      = UPOL | AFF | NEG deriving (Eq, Show, Read)
-- For verbs
data Tense         = UTEN | PST | PRS | FUT
                   | APRS | APST
                   | AFUT | AFUT1 | AFUT2 | AFUT3
                   | PPRS | PFUT
                   | PPST | PPST1 | PPST2 | PPST3
                   | PSTPER | PRSPER | FUTPER deriving (Eq, Show, Read)
data Aspect        = UASP | NNPROG | PFV | IPFV | HAB | CONT | NPROG | PROG deriving (Eq, Show, Read)
data Mood          = UMOO | IND | IRR | DEO | IMP | JUS | OPT | EPIS | SBJV | POT | COND deriving (Eq, Show, Read)
data Voice         = UVOI | ACTIVE | MIDDLE | PASSIVE deriving (Eq, Show, Read)
data Evidentiality = UEVI | EXP | VIS | NVIS | AUD | INFER | REP | HSY | QUO | ASS deriving (Eq, Show, Read)
data Transitivity  = UTRA | NTRANS | TRANS | MTRANS | DITRANS deriving (Eq, Show, Read)
data Volition      = UVOL | VOL | NVOL deriving (Eq, Show, Read)

-- Particle/Affix system
data ManifestSystem = ManifestSystem
                    { manSysLC :: LexCat
                    , manSysType :: ManifestType
                    , manSysCombos :: [(Morpheme, (Express Gender, Express Animacy, Express Case, Express Number, Express Definiteness, Express Specificity, Express Topic, Express Person, Express Honorific, Express Polarity, Express Tense, Express Aspect, Express Mood, Express Voice, Express Evidentiality, Express Transitivity, Express Volition))]
                    } deriving (Eq, Show)
