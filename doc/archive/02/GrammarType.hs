module GrammarType
(
MorphosyntacticSystem(..)
, Gender(..)
, GenderSystem(..)
, Animacy(..)
, AnimacySystem(..)
, Case(..)
, CaseSystem(..)
, Number(..)
, NumberSystem(..)
, Honorific(..)
, HonorificSystem(..)
, Definiteness(..)
, DefinitenessSystem(..)
, Specificity(..)
, SpecificitySystem(..)
, Declension(..)
, DeclensionPattern(..)
, Key(..)
) where

import Prelude hiding (Word)
import Data.List

import PhonologyType


-- Grammatical Features
data Grammatical = Grammatical

-- Morphosyntactic Features
data MorphosyntacticSystem = MorphosyntacticSystem
    { genderSystem :: GenderSystem
    , animacySystem :: AnimacySystem
    , caseSystem :: CaseSystem
    , numberSystem :: NumberSystem
    , honorificSystem :: HonorificSystem
    , definitenessSystem :: DefinitenessSystem
    , specificitySystem :: SpecificitySystem    
    } deriving (Show)

data Gender = M | F | COM | N  deriving (Eq, Show, Read)
data GenderSystem = GenderSystem [Gender] deriving (Show, Read)

data Animacy = AN | HUM | NHUM | ZO | INAN deriving (Eq, Show, Read)
data AnimacySystem = AnimacySystem [Animacy] deriving (Show, Read)

data Case = NOM | ACC | ERG | GEN | DAT | LOC | PREP | ABL | INS | VOC deriving (Eq, Show, Read)
data CaseSystem = CaseSystem [Case] deriving (Show, Read)

data Number = SG | DU | TRI | PA | PL deriving (Eq, Show, Read)
data NumberSystem = NumberSystem [Number] deriving (Show, Read)

data Honorific = FAM | FORM deriving (Eq, Show, Read)
data HonorificSystem = HonorificSystem [Honorific] deriving (Show, Read)

data Definiteness = DEF | INDF deriving (Eq, Show, Read)
data DefinitenessSystem = DefinitenessSystem [Definiteness] deriving (Show, Read)

data Specificity = SPEC | NSPEC deriving (Eq, Show, Read)
data SpecificitySystem = SpecificitySystem [Specificity] deriving (Show, Read)

--data ExponentType = DUP | PREFIX | SUFFIX | CIRCUMFIX deriving (Show, Read)
--data Exponent a = Exponent a ExponenetType [Char] [Char] deriving (Show, Read)


type Key = (Gender, Animacy, Case, Number, Honorific, Definiteness, Specificity)
data Declension = Declension [(Key, [Phoneme])] deriving (Show)
type DeclensionPattern = ((Int,[(Gender,Phoneme)])
                        ,(Int,[(Animacy,Phoneme)])
                        ,(Int,[(Case,Phoneme)])
                        ,(Int,[(Number,Phoneme)])
                        ,(Int,[(Honorific,Phoneme)])
                        ,(Int,[(Definiteness,Phoneme)])
                        ,(Int,[(Specificity,Phoneme)])
                        )

-- Morphosemantic Features
--data Tense = Tense
--data Aspect = Aspect
--data Mood = Mood
--data Polarity = Polarity
--data Transitivity = Transitivity

