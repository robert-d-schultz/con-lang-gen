module Data.Word
( Word(..)
, Morpheme(..)
, Meaning(..)
, InflType(..)
, Syllable(..)
, ConsCluster
, LemmaMorphemes(..)
) where

import ClassyPrelude hiding (Word)
import Data.List (elemIndex)

import Data.Phoneme
import Data.Inflection

-- A Word is one or more Morphemes
-- Words stand alone
-- Clitics attach themselves to previous/next Words
data Word = Word [Morpheme]
          | Proclitic AllExpress [Morpheme]
          | Enclitic [Morpheme] deriving (Eq, Show)

-- Morphemes can be either syllabized or unsyllablized
-- Need syllabized Morphemes to carry tone and stress
-- Need raw phonemes for unsyllabizable inflectional morphemes and stuff
data Morpheme = MorphemeS { getMeaning :: Meaning, getSylls :: [Syllable] }
              | MorphemeP { getMeaning :: Meaning, getPhonemes :: [Phoneme] } deriving (Eq, Show)

-- Meanings are basically just English-language "senses"
-- Eventually Verbs will need to carry their Theta roles
data Meaning = Meaning { getLC :: LexCat, getStr :: Text }
             | InflMeaning { getLC :: LexCat, getInflType :: InflType, getAllExpress :: AllExpress } deriving (Eq, Show)
             --[ThetaRole]
--data ThetaRole = Agent | Experiencer | Theme | Patient deriving (Eq, Enum, Read, Show)

data Syllable = Syllable
              { getOnset :: [Phoneme]
              , getNucleus :: Phoneme
              , getCoda :: [Phoneme]
              , getTone :: Tone
              , getStress :: Stress
              } deriving (Eq, Ord, Show)

type ConsCluster = [Phoneme]

-- This holds the morphemes for a specific POS/type
-- A language would have a list of these, [ManifestSystem]
{-data ManifestSystem = ManifestSystem
                    { manSysLC :: LexCat
                    , manSysType :: InflType
                    , manSysCombos :: [Morpheme]
                    } deriving (Eq, Show)-}

type LemmaMorphemes = [Morpheme]
