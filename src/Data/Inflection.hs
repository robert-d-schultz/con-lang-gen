{-# LANGUAGE StandaloneDeriving #-}
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
import Data.Other

-- Used for 18-tuples
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q) => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q) = showTuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, shows i, shows j, shows k, shows l, shows m, shows n, shows o, shows p, shows q]

showTuple :: [ShowS] -> ShowS
showTuple ss = showChar '('
              . foldr1 (\s r -> s . showChar ',' . r) ss
              . showChar ')'

deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o, Eq p, Eq q) => Eq (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)

-- Manifest (list of places) (list of stuff that manifests there)
data Manifest a = NoManifest | Manifest { getManPlace :: [(LexCat, ManifestType, Int)], getManStuff :: [a] } deriving (Eq, Read)

instance Show a => Show (Manifest a) where
  show NoManifest = ""
  show (Manifest _ []) = ""
  show (Manifest _ [x]) = show x
  show (Manifest _ [x,y]) = show x ++ " and " ++ show y
  show (Manifest _ (x:xs)) = intercalate ", " (map show xs) ++ ", and " ++ show x

data Express a  = NoExpress
                | Express { getExp :: a } deriving (Eq, Read)

instance Show a => Show (Express a) where
  show NoExpress = ""
  show (Express x) = show x

data ManifestType = Particle | Prefix | Suffix deriving (Eq, Show, Read)

-- Lexical categories
data LexCat = Comp | Infl | Verb | Det | Noun | Adpo | Adj | Adv | Pron
            | Agen | Obj | Subj | Don | Them | Rec -- arguments for verbs
              deriving (Eq, Enum, Read)
instance Show LexCat where
  show lc = case lc of Noun -> "Noun"
                       Adj  -> "Adjective"
                       Adv  -> "Adverb"
                       Adpo -> "Adposition"
                       Verb -> "Verb"
                       Agen -> "Agent"
                       Obj -> "Object"
                       Subj -> "Subject"
                       Don -> "Donor"
                       Them -> "Theme"
                       Rec -> "Recipient"

-- Inflection system
data InflectionMap = InflectionMap
                      { getGenSys :: Manifest Gender
                      , getAniSys :: Manifest Animacy
                      , getCasSys :: Manifest Case
                      , getNumSys :: Manifest Number
                      , getDefSys :: Manifest Definiteness
                      , getSpeSys :: Manifest Specificity
                      , getTopSys :: Manifest Topic
                      , getPerSys :: Manifest Person
                      , getHonSys :: Manifest Honorific
                      , getPolSys :: Manifest Polarity
                      , getTenSys :: Manifest Tense
                      , getAspSys :: Manifest Aspect
                      , getMooSys :: Manifest Mood
                      , getVoiSys :: Manifest Voice
                      , getEviSys :: Manifest Evidentiality
                      , getTraSys :: Manifest Transitivity
                      , getVolSys :: Manifest Volition
                      }  deriving (Eq, Show, Read)

-- Grammatical categories
-- For nouns
data Gender        = UGEN | M | F | COM | N  deriving (Eq, Read, Show)
data Animacy       = UANI | AN | HUM | NHUM | ZO | INAN deriving (Eq, Read, Show)
data Case          = UCAS
                   | DIR | DIR2
                   | INTR | MTR | DTR | TR
                   | NOM | NOM2
                   | ACC | ACC2 | ACC3
                   | OBJ | OBJ2
                   | ERG | ERG2
                   | ABS | ABS2 | ABS3 | ABS4
                   | PEG | SEC | DAT

                   | PREP
                   | LAT | ELA | DEL | ABL | EXESS
                   | LOC
                   | SEP

                   | COMP | EQU | IDEN | ABE
                   | INS | COMIT | INSCOMIT | ORN | BEN
                   | CAUS | DISTR
                   | GEN | POSS | PART
                   | VOC deriving (Eq, Read)

-- "Case" signals what the attached word's function is in the phrase
-- The morpho-syntactic cases signal which verb argument they are
-- Genitive cases signal that a noun is modifying another noun
--   * Inalienable Possessive, Alienable Possessive, Possesed, Partitive
-- Prepositional case signals a relationship between a adposition and noun
--   * English's Oblique case is a merger Accusative and Prepositional, I think
--   * I suppose you would have either Prepostional case + actual prepositions OR a bunch of Location/Motion-related cases
--   * Although maybe both could work, and you would know which prepositions go with which NPs
-- Locative (at), Seperative (away), and Lative (to) cases
--   * These type of cases are really granular
--   * Do these represent VP-NP or VP-NP-NP relations?
--   * I suppose you'd have pairs of these working (Like Seperative and Lative) to show motion away from something to something else

-- General cases:
-- Morphosyntactic cases (NP-VP) showing theta roles
-- Genitive (NP-NP cases) showing relations
-- Seperative, Locative, Lative (VP-NP cases) showing a motion relative to a thing
-- Prepositional (PP-NP cases) showing that the noun is what the preposition is talking about
-- Partitive? (DP-NP case?)
{-
data Case = CaseGroup Text [Case]
          | Case Text

cases = [ Case "ELA"
        , Case "DEL"
        , Case "ABL"
        , Case "EXESS"
        , CaseGroup "LAT" [Case "ELA", Case "DEL", Case "ABL", Case "EXESS"]
        ]


-- Returns the arguments that each Morphosyntactic Case applies to
getCases c = case c of INTR  -> [Subj]
                       ERG2  -> [Agen]
                       ACC2  -> [Obj]
                       PEG   -> [Don]
                       SEC   -> [Them]
                       DAT   -> [Rec]

                       NOM2  -> [Subj, Agen]
                       ABS2  -> [Subj, Obj]
                       ERG   -> [Agen, Don]
                       ACC3  -> [Obj, Rec]
                       ACC   -> [Obj, Them]

                       MTR   -> [Agen, Obj]
                       OBJ2  -> [Them, Rec]

                       NOM   -> [Subj, Agen, Don]
                       ABS3  -> [Subj, Obj, Rec]

                       ABS4  -> [Subj, Obj, Them]

                       DTR   -> [Don, Them, Rec]

                       DIR2  -> [Subj, Agen, Obj]
                       OBJ   -> [Obj, Them, Rec]

                       ABS   -> [Subj, Obj, Them, Rec]

                       TR    -> [Agen, Obj, Don, Them, Rec]

                       DIR   -> [Subj, Agen, Obj, Don, Them, Rec]


                       --          Subj
                       --     Agent   Object
                       -- Donor   Theme   Recipient

-- Just a bunch of likely alignments
data Alignment = Alignment Text [Case]
alignment = [ Alignment "Nominative-objective" [NOM, OBJ]
            , Alignment "Nominative-accusative (Secundative)" [NOM, SEC, ACC3]
            , Alignment "Nominative-accusative (Indirective)" [NOM, ACC, DAT]
            , Alignment "Ergative-absolutive" [ERG, ABS]
            , Alignment "Ergative-absolutive (Secundative)" [ERG, SEC, ABS3]
            , Alignment "Ergative-absolutive (Indirective)" [ERG, SEC, ABS4, DAT]
            , Alignment "Transitive" [INTR, TR]
            , Alignment "Mono-Ditransitive" [INTR, MTR, DTR]
            , Alignment "Tripartite" [INTR, ERG, OBJ]
            , Alignment "Quadpartite (Secundative)" [INTR, ERG, SEC, ACC3]
            , Alignment "Quadpartite (Indirective)" [INTR, ERG, ACC, DAT]
            , Alignment "Hexpartite" [INTR, ERG2, OBJ, PEG, SEC, DAT]
            , Alignment "Direct" [DIR]
            , Alignment "Ditransitive" [DIR2, DTR]
            ]
-}

data Number        = UNUM | SG | DU | TRI | PA | PL deriving (Eq, Read, Show)
data Definiteness  = UDEF | DEF | INDF deriving (Eq, Read, Show)
data Specificity   = USPE | SPEC | NSPEC deriving (Eq, Read, Show)
data Topic         = UTOP | TOP | NTOP deriving (Eq, Read, Show)
data Person        = UPER | FIRST | FSTINCL | FSTEXCL | SECOND | THIRD | THRDPROX | THRDOBV deriving (Eq, Read, Show)
-- For nouns and verbs
data Honorific     = UHON | FAM | NEU | FORM deriving (Eq, Read, Show)
data Polarity      = UPOL | AFF | NEG deriving (Eq, Read, Show)
-- For verbs
data Tense         = UTEN | PST | PRS | FUT
                   | APRS | APST
                   | AFUT | AFUT1 | AFUT2 | AFUT3
                   | PPRS | PFUT
                   | PPST | PPST1 | PPST2 | PPST3
                   | PSTPER | PRSPER | FUTPER deriving (Eq, Read, Show)
data Aspect        = UASP | NNPROG | PFV | IPFV | HAB | CONT | NPROG | PROG deriving (Eq, Read, Show)
data Mood          = UMOO | IND | IRR | DEO | IMP | JUS | OPT | EPIS | SBJV | POT | COND deriving (Eq, Read, Show)
data Voice         = UVOI | ACTIVE | MIDDLE | PASSIVE deriving (Eq, Read, Show)
data Evidentiality = UEVI | EXP | VIS | NVIS | AUD | INFER | REP | HSY | QUO | ASS deriving (Eq, Read, Show)
data Transitivity  = UTRA | NTRANS | TRANS | MTRANS | DITRANS deriving (Eq, Read, Show)
data Volition      = UVOL | VOL | NVOL deriving (Eq, Read, Show)
{-
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
-}

instance Show Case where
  show cas = case cas of ACC -> "Accusative"
                         ACC2 -> "Accusative"
                         ACC3 -> "Accusative"
                         ERG -> "Ergative"
                         ERG2 -> "Ergative"
                         PEG -> "Pegative"
                         DAT -> "Dative"
                         SEC -> "Secundative"
                         NOM -> "Nominative"
                         NOM2 -> "Nominative"
                         ABS -> "Absolutive"
                         ABS2 -> "Absolutive"
                         ABS3 -> "Absolutive"
                         ABS4 -> "Absolutive"
                         INTR -> "Intransitive"
                         MTR -> "Monotransitive"
                         DTR -> "Ditransitive"
                         TR -> "Transitive"
                         DIR -> "Directive"
                         DIR2 -> "Directive"
                         OBJ -> "Objective"
                         PREP -> "Prepositional"
{-
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
-}
-- Particle/Affix system
data ManifestSystem = ManifestSystem
                    { manSysLC :: LexCat
                    , manSysType :: ManifestType
                    , manSysCombos :: [(SyllWord, AllExpress)]
                    } deriving (Eq, Show)

type AllExpress = ( Express Gender
                  , Express Animacy
                  , Express Case
                  , Express Number
                  , Express Definiteness
                  , Express Specificity
                  , Express Topic
                  , Express Person
                  , Express Honorific
                  , Express Polarity
                  , Express Tense
                  , Express Aspect
                  , Express Mood
                  , Express Voice
                  , Express Evidentiality
                  , Express Transitivity
                  , Express Volition
                  )
