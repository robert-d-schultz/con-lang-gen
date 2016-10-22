module GrammarType
( Declension(..)
, AgglSystem(..)
, Template(..)
, Maybe(..)
, GrammarSystem(..)
, Gender(..)
, Animacy(..)
, Case(..)
, Number(..)
, Honorific(..)
, Definiteness(..)
, Specificity(..)
) where

import Prelude
import OtherData

-- Declension stuff
data Declension = Declension [(Syllable, (Maybe Gender, Maybe Animacy, Maybe Case, Maybe Number, Maybe Honorific, Maybe Definiteness, Maybe Specificity))] deriving (Show)

data AgglSystem = AgglSystem (Maybe [(Syllable, Gender)]) (Maybe [(Syllable, Animacy)]) (Maybe [(Syllable, Case)]) (Maybe [(Syllable, Number)]) (Maybe [(Syllable, Honorific)]) (Maybe [(Syllable, Definiteness)]) (Maybe [(Syllable, Specificity)]) deriving (Show)

data Template = Template (Int, Int, Int, Int, Int, Int, Int) deriving (Show) -- 0 = unassigned, 1 = C1, 2 = V, 3 = C2

data GrammarSystem = GrammarSystem
    { gSys :: Maybe [Gender]
    , aSys :: Maybe [Animacy]
    , cSys :: Maybe [Case]
    , nSys :: Maybe [Number]
    , hSys :: Maybe [Honorific]
    , dSys :: Maybe [Definiteness]
    , sSys :: Maybe [Specificity]
    } deriving (Show)

data Gender       = M | F | COM | N  deriving (Eq, Show, Read)

data Animacy      = AN | HUM | NHUM | ZO | INAN deriving (Eq, Show, Read)

data Case         = NOM | ACC | ERG | GEN | DAT | LOC | PREP | ABL | INS | VOC deriving (Eq, Show, Read)

data Number       = SG | DU | TRI | PA | PL deriving (Eq, Show, Read)

data Honorific    = FAM | FORM deriving (Eq, Show, Read)

data Definiteness = DEF | INDF deriving (Eq, Show, Read)

data Specificity  = SPEC | NSPEC deriving (Eq, Show, Read)

--data ExponentType = DUP | PREFIX | SUFFIX | CIRCUMFIX deriving (Show, Read)
--data Exponent a = Exponent a ExponenetType [Char] [Char] deriving (Show, Read)

--type Key = (Gender, Animacy, Case, Number, Honorific, Definiteness, Specificity)

-- Conjugation stuff
--data Tense = Tense
--data Aspect = Aspect
--data Mood = Mood
--data Polarity = Polarity
--data Transitivity = Transitivity
