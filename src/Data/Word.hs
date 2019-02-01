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

-- Words have a tree-like structure where leaves are morphemes
data Word = Word { getMeaning :: Meaning, getLeft :: Morpheme, getRight :: Morpheme }
          -- Syllabic morpheme (Sequence of syllables)
          | MorphemeS { getMeaning :: Meaning, getMorphType :: MorphType, getSylls :: [Syllable] }
          -- Phonemic morpheme (Unstructured sequence of phonemes)
          | MorphemeP { getMeaning :: Meaning, getMorphType :: MorphType, getPhonemes :: [Phoneme] }
          -- Consonantal morpheme (Disconnected sequences of consonants, ie. Semitic root)
          | MorphemeC { getMeaning :: Meaning, getMorphType :: MorphType, getRadicals :: [[Phoneme]] }
          -- Vocallic morpheme (Disconnected sequences of (typically) vowels, ie. Patterns/transfixes)
          | MorphemeV { getMeaning :: Meaning, getMorphType :: MorphType, getPatterns :: [Syllable] } deriving (Eq, Show)
-- Clitics attach themselves to previous/next Words
-- They should probably be represented as a MorphType
       -- | Proclitic Meaning [Syllable]
       -- | Enclitic Meaning [Syllable] deriving (Eq, Show)

-- Morphemes can be either syllabized or unsyllablized
-- Need syllabized Morphemes to carry tone and stress
-- Need raw phonemes for unsyllabizable inflectional morphemes and stuff
type Morpheme = Word

-- Meanings are basically just English-language "senses"
-- Eventually Verbs will need to carry their Theta roles
data Meaning = Meaning { getLC :: LexCat, getStr :: Text }
             | InflMeaning { getLC :: LexCat, getAllExpress :: GramCatExpress }
             | DeriMeaning { getLC1 :: LexCat, getLC2 :: LexCat, getStr :: Text } deriving (Eq, Show, Read)
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
