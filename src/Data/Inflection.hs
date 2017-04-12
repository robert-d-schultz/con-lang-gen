{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.Inflection
( Manifest(..)
, Express(..)
, ManifestType(..)
, LexCat(..)
, InflectionMap(..)
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
, AllExpress(..)
) where

import ClassyPrelude
import Prelude (ShowS, shows, showChar, foldr1)

import Data.Phoneme

-- Used for 18-tuples
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q) => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) = showTuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, shows i, shows j, shows k, shows l, shows m, shows n, shows o, shows p, shows q]

showTuple :: [ShowS] -> ShowS
showTuple ss = showChar '('
              . foldr1 (\s r -> s . showChar ',' . r) ss
              . showChar ')'

deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o, Eq p, Eq q) => Eq (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)

-- Manifest (list of places) (list of stuff that manifests there)
data Manifest a = NoManifest | Manifest [(LexCat, ManifestType, Int)] [a] deriving (Eq, Read)

instance Show a => Show (Manifest a) where
  show NoManifest = ""
  show (Manifest _ []) = ""
  show (Manifest _ [x]) = show x
  show (Manifest _ (x:xs)) = intercalate ", " (map show xs) ++ ", and " ++ show x

data Express a  = NoExpress
                | Express { getExp :: a } deriving (Eq, Read)

instance Show a => Show (Express a) where
  show NoExpress = ""
  show (Express x) = show x

data ManifestType = Particle | Prefix | Suffix deriving (Eq, Show, Read)

data LexCat = Comp | Infl | Verb | Det | Noun | Adpo | Adj | Adv | Obj | Subj | Pron deriving (Eq, Enum, Read)
instance Show LexCat where
  show lc = case lc of Subj -> "Subject"
                       Obj  -> "Object"
                       Noun -> "Noun"
                       Adj  -> "Adjective"
                       Adv  -> "Adverb"
                       Adpo -> "Adposition"
                       Verb -> "Verb"

-- Inflection system for nouns
data InflectionMap = InflectionMap
                      { genSys :: Manifest Gender
                      , aniSys :: Manifest Animacy
                      , casSys :: Manifest Case
                      , numSys :: Manifest Number
                      , defSys :: Manifest Definiteness
                      , speSys :: Manifest Specificity
                      , topSys :: Manifest Topic
                      , perSys :: Manifest Person
                      , honSys :: Manifest Honorific
                      , polSys :: Manifest Polarity
                      , tenSys :: Manifest Tense
                      , aspSys :: Manifest Aspect
                      , mooSys :: Manifest Mood
                      , voiSys :: Manifest Voice
                      , eviSys :: Manifest Evidentiality
                      , traSys :: Manifest Transitivity
                      , volSys :: Manifest Volition
                      }  deriving (Eq, Show, Read)

-- Grammatical categories
-- For nouns
data Gender        = UGEN | M | F | COM | N  deriving (Eq, Read)
data Animacy       = UANI | AN | HUM | NHUM | ZO | INAN deriving (Eq, Read)
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
                   | VOC deriving (Eq, Read)
data Number        = UNUM | SG | DU | TRI | PA | PL deriving (Eq, Read)
data Definiteness  = UDEF | DEF | INDF deriving (Eq, Read)
data Specificity   = USPE | SPEC | NSPEC deriving (Eq, Read)
data Topic         = UTOP | TOP | NTOP deriving (Eq, Read)
data Person        = UPER | FIRST | FSTINCL | FSTEXCL | SECOND | THIRD | THRDPROX | THRDOBV deriving (Eq, Read)
-- For nouns and verbs
data Honorific     = UHON | FAM | NEU | FORM deriving (Eq, Read)
data Polarity      = UPOL | AFF | NEG deriving (Eq, Read)
-- For verbs
data Tense         = UTEN | PST | PRS | FUT
                   | APRS | APST
                   | AFUT | AFUT1 | AFUT2 | AFUT3
                   | PPRS | PFUT
                   | PPST | PPST1 | PPST2 | PPST3
                   | PSTPER | PRSPER | FUTPER deriving (Eq, Read)
data Aspect        = UASP | NNPROG | PFV | IPFV | HAB | CONT | NPROG | PROG deriving (Eq, Read)
data Mood          = UMOO | IND | IRR | DEO | IMP | JUS | OPT | EPIS | SBJV | POT | COND deriving (Eq, Read)
data Voice         = UVOI | ACTIVE | MIDDLE | PASSIVE deriving (Eq, Read)
data Evidentiality = UEVI | EXP | VIS | NVIS | AUD | INFER | REP | HSY | QUO | ASS deriving (Eq, Read)
data Transitivity  = UTRA | NTRANS | TRANS | MTRANS | DITRANS deriving (Eq, Read)
data Volition      = UVOL | VOL | NVOL deriving (Eq, Read)

-- show instances
instance Show Gender where
  show gen = case gen of UGEN -> "Unknown"
                         M   -> "Masculine"
                         F   -> "Feminine"
                         COM -> "Common"
                         N   -> "Neuter"

instance Show Animacy where
  show ani = case ani of UANI -> "Unknown"
                         AN   -> "Animate"
                         HUM  -> "Human"
                         NHUM -> "Non-Human"
                         ZO   -> "Animal"
                         INAN -> "Inanimate"

instance Show Case where
  show cas = case cas of UCAS -> "Unknown"
                         INTR -> "Intransitive"
                         ACC -> "Accusative"
                         ERG -> "Ergative"
                         ERG2 -> "Ergative"
                         PEG -> "Pegative"
                         INDIR -> "Indirective"
                         SEC -> "Secundative"
                         NOM -> "Nominative"
                         NOM2 -> "Nominative"
                         ABS -> "Absolutive"
                         ABS2 -> "Absolutive"
                         ABS3 -> "Absolutive"
                         MTR -> "Monotransitive"
                         DIR -> "Directive"
                         PRIM -> "Primative"
                         DTR -> "Ditransitive"
                         OBJ -> "Objective"
                         DRT1 -> "Direct"
                         DRT2 -> "Direct"
                         TR -> "Transitive"
                         ADP -> "Adpositional"
                         PREP -> "Prepositional"
                         POST -> "Postpositional"
                         OBL1 -> "Oblique"
                         OBL2 -> "Oblique"
                         OBL3 -> "Oblique"
                         OBL4 -> "Oblique"
                         OBL5 -> "Oblique"
                         OBL6 -> "Oblique"

instance Show Number where
  show num = case num of UNUM -> "Unknown"
                         SG  -> "Singular"
                         DU  -> "Dual"
                         TRI -> "Trial"
                         PA  -> "Paucal"
                         PL  -> "Plural"

instance Show Definiteness where
  show def = case def of UDEF -> "Unknown"
                         DEF  -> "Definite"
                         INDF -> "Indefinite"

instance Show Specificity where
  show spe = case spe of USPE -> "Unknown"
                         SPEC  -> "Specific"
                         NSPEC -> "Nonspecific"

instance Show Topic where
  show top = case top of UTOP -> "Unknown"
                         TOP  -> "Topic"
                         NTOP -> "Not topic"

instance Show Person where
  show per = case per of UPER -> "Unknown"
                         FIRST  -> "First"
                         FSTINCL -> "First inclusive"
                         FSTEXCL -> "First exclusive"
                         SECOND -> "Second"
                         THIRD  -> "Third"
                         THRDPROX  -> "Proximate"
                         THRDOBV  -> "Obviative"

instance Show Honorific where
  show hon = case hon of UHON  -> "Unknown"
                         FAM   -> "Informal"
                         NEU   -> "Neutral"
                         FORM  -> "Formal"

instance Show Polarity where
  show pol = case pol of UPOL -> "Unknown"
                         AFF -> "Affirmative"
                         NEG -> "Negative"

instance Show Tense where
  show ten = case ten of UTEN -> "Unknown"
                         PST  -> "Simple past"
                         PRS  -> "Simple present"
                         FUT  -> "Simple future"
                         APRS -> "Anterior present"
                         APST -> "Anterior past"
                         AFUT -> "Anterior future"
                         AFUT1 -> "Anterior future"
                         AFUT2 -> "Anterior future"
                         AFUT3 -> "Anterior future"
                         PPRS -> "Posterior present"
                         PFUT -> "Posterior future"
                         PPST -> "Posterior past"
                         PPST1 -> "Posterior past"
                         PPST2 -> "Posterior past"
                         PPST3 -> "Posterior past"
                         PSTPER  -> "Past perfect"
                         PRSPER  -> "Present perfect"
                         FUTPER  -> "Future perfect"

instance Show Aspect where
  show asp = case asp of UASP -> "Unknown"
                         NNPROG -> "Not progressive"
                         PFV  -> "Perfective"
                         IPFV -> "Imperfective"
                         HAB -> "Habitual"
                         CONT -> "Continuous"
                         NPROG -> "Non-progressive"
                         PROG -> "Progressive"


instance Show Mood where
  show moo = case moo of UMOO -> "Unknown"
                         IND  -> "Indicative"
                         IRR  -> "Irrealis"
                         DEO  -> "Deontic"
                         IMP  -> "Imperative"
                         JUS  -> "Jussive"
                         OPT  -> "Optative"
                         EPIS -> "Epistemic"
                         SBJV -> "Subjunctive"
                         POT  -> "Potential"
                         COND -> "Conditional"

instance Show Voice where
  show voi = case voi of UVOI -> "Unknown"
                         ACTIVE  -> "Active"
                         MIDDLE  -> "Middle"
                         PASSIVE -> "Passive"

instance Show Evidentiality where
  show evi = case evi of UEVI  -> "Unknown"
                         EXP   -> "Witness"
                         VIS   -> "Visual"
                         NVIS  -> "Non-visual"
                         AUD   -> "Auditory"
                         INFER -> "Inferential"
                         REP   -> "Reportative"
                         HSY   -> "Hearsay"
                         QUO   -> "Quotative"
                         ASS   -> "Assumed"

instance Show Transitivity where
  show tra = case tra of UTRA    -> "Unknown"
                         NTRANS  -> "Intransitive"
                         TRANS   -> "Transitive"
                         MTRANS  -> "Monotransitive"
                         DITRANS -> "Ditransitive"

instance Show Volition where
  show vol = case vol of UVOL -> "Unknown"
                         VOL  -> "Intended"
                         NVOL -> "Unintended"

-- Particle/Affix system
data ManifestSystem = ManifestSystem
                    { manSysLC :: LexCat
                    , manSysType :: ManifestType
                    , manSysCombos :: [(Morpheme, AllExpress)]
                    } deriving (Eq, Show)

type AllExpress = (Express Gender, Express Animacy, Express Case, Express Number, Express Definiteness, Express Specificity, Express Topic, Express Person, Express Honorific, Express Polarity, Express Tense, Express Aspect, Express Mood, Express Voice, Express Evidentiality, Express Transitivity, Express Volition)
