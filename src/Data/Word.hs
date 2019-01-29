module Data.Word
( Word(..)
, Morpheme(..)
, Meaning(..)
, Syllable(..)
, ConsCluster
) where

import ClassyPrelude hiding (Word)
import Data.List (elemIndex)

import Data.Phoneme
import Data.Inflection

-- A Word is one or more Morphemes
-- Words stand alone
-- Clitics attach themselves to previous/next Words
data Word = Word { getMeaning :: Meaning, getLeft :: Morpheme, getRight :: Morpheme }
          | MorphemeS { getMeaning :: Meaning, getMorphType :: MorphType, getSylls :: [Syllable] }
          | MorphemeP { getMeaning :: Meaning, getMorphType :: MorphType, getPhonemes :: [Phoneme] }
          | SemiticRoot { getMeaning :: Meaning, getMorphType :: MorphType, getRadicals :: [[Phoneme]] } deriving (Eq, Show)

          -- | Proclitic AllExpress [Morpheme]
          -- | Enclitic AllExpress [Morpheme] deriving (Eq, Show)

-- Morphemes can be either syllabized or unsyllablized
-- Need syllabized Morphemes to carry tone and stress
-- Need raw phonemes for unsyllabizable inflectional morphemes and stuff
type Morpheme = Word

-- Meanings are basically just English-language "senses"
-- Eventually Verbs will need to carry their Theta roles
data Meaning = Meaning { getLC :: LexCat, getStr :: Text }
             | InflMeaning { getLC :: LexCat, getAllExpress :: AllExpress } deriving (Eq, Show)
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
