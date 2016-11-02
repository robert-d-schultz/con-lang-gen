{-# LANGUAGE StandaloneDeriving #-}
module InflectionData
( Manifest(..)
, ManifestType(..)
, InflectionSystem(..)
, Gender(..)
, Animacy(..)
, Case(..)
, Number(..)
, Definiteness(..)
, Specificity(..)
, Topic(..)
, Person(..)
, Clusivity(..)
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
, LexicalCategory(..)
, AgreementSystem(..)
, Agreement(..)
) where

import Prelude
import PhonemeData
import OtherData

-- Used for 18-tuples
instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p, Show q, Show r) => Show (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) where
  showsPrec _ (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) = showTuple [shows a, shows b, shows c, shows d, shows e, shows f, shows g, shows h, shows i, shows j, shows k, shows l, shows m, shows n, shows o, shows p, shows q, shows r]

showTuple :: [ShowS] -> ShowS
showTuple ss = showChar '('
              . foldr1 (\s r -> s . showChar ',' . r) ss
              . showChar ')'

deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o, Eq p, Eq q, Eq r) => Eq (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)

-- How does the grammatical category manifest?
-- NoManifest - there is no concept of this in the language
-- Particle - category uses particles to manifest (the, an, a for definiteness/specificity in English?)
-- Exponent - manifests by changing the word


data Manifest a = NoManifest | Manifest [(LexicalCategory, ManifestType, Int)] a deriving (Eq, Show, Read)

data ManifestType = PreParticle | Prefix | Suffix | PostParticle deriving (Eq, Show, Read)

data LexicalCategory = Sub | Obj | Adj | Adv | Prep | Verb deriving (Eq, Show, Read)


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
                      , cluSys :: Manifest [Clusivity]
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
data Gender        = M | F | COM | N  deriving (Eq, Show, Read)
data Animacy       = AN | HUM | NHUM | ZO | INAN deriving (Eq, Show, Read)
data Case          = INTR | ACC | ERG | PEG | INDIR | SEC
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
                   | VOC

                   deriving (Eq, Show, Read)
data Number        = SG | DU | TRI | PA | PL deriving (Eq, Show, Read)
data Definiteness  = DEF | INDF deriving (Eq, Show, Read)
data Specificity   = SPEC | NSPEC deriving (Eq, Show, Read)
data Topic         = TOP | NTOP deriving (Eq, Show, Read)
data Person        = FIRST | SECOND | THIRD deriving (Eq, Show, Read)
data Clusivity     = INCL | EXCL deriving (Eq, Show, Read)
-- For nouns and verbs
data Honorific     = FAM | FORM deriving (Eq, Show, Read)
data Polarity      = AFF | NEG deriving (Eq, Show, Read)
-- For verbs
data Tense         = PST | REM | REC | NPST | PRS | NFUT | FUT | IMMF | REMF deriving (Eq, Show, Read)
data Aspect        = PFV | IPFV deriving (Eq, Show, Read)
data Mood          = IND | IRR | DEO | IMP | JUS | OPT | EPIS | SBJV | POT | COND deriving (Eq, Show, Read)
data Voice         = ACTIVE| MIDDLE | PASSIVE deriving (Eq, Show, Read)
data Evidentiality = EXP | VIS | NVIS | AUD | INFER | REP | HSY | QUO | ASS deriving (Eq, Show, Read)
data Transitivity  = NTRANS | TRANS | DITRANS deriving (Eq, Show, Read)
data Volition      = VOL | NVOL deriving (Eq, Show, Read)

-- Particle/Exponent system
data ManifestSystem = ManifestSystem LexicalCategory ManifestType [(Morpheme, (Manifest Gender, Manifest Animacy, Manifest Case, Manifest Number, Manifest Definiteness, Manifest Specificity, Manifest Topic, Manifest Person, Manifest Clusivity, Manifest Honorific, Manifest Polarity, Manifest Tense, Manifest Aspect, Manifest Mood, Manifest Voice, Manifest Evidentiality, Manifest Transitivity, Manifest Volition))] deriving (Eq, Show)

-- Agreement system

data AgreementSystem = AgreementSystem [Agreement] deriving (Eq, Show, Read)
data Agreement = Agreement LexicalCategory LexicalCategory deriving (Eq, Show, Read)
