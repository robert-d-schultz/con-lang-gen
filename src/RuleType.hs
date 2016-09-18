-- Data types for allophones (potentially anything though)
module RuleType
( PhonologicalRule(..)
, PhoneChange(..)
) where

import Prelude hiding (Word)
import Data.List
import PhonemeType

data ConVowel a b = C a | V b

--data Boundary a b c d = Phoneme a | Syllable b | Morpheme c | Word d

data PhonologicalRule = PhonologicalRule
    { change :: PhoneChange
    , environment :: Boundary PhonePhonemeEnvironment PhonePhonemeEnvironment
    } deriving (Eq, Show, Read)

data PhoneChange = PhoneChange
    { from :: ConVowel Consonant Vowel
    , to :: ConVowel Consonant Vowel
    } deriving (Eq, Show, Read)

data PhonePhonemeEnvironment = PhonePhonemeEnvironment
  { previousPhoneme :: ConVowel Consonant Vowel
  , nextPhoneme :: ConVowel Consonant Vowel
  } deriving (Eq, Show, Read)

data PhoneSyllableEnvironment = PhoneSyllableEnvironment
  { previousSyllable :: Syllable
  , nextSyllable :: Syllable
  } deriving (Eq, Show, Read)

data PhoneMorphemeEnvironment = PhoneMorphemeEnvironment
  { previousMorpheme :: Morpheme
  , nextMorpheme :: Morpheme
  } deriving (Eq, Show, Read)

data PhoneWordEnvironment = PhoneWordEnvironment
  { previousWord :: Word
  , nextWord :: Word
  } deriving (Eq, Show, Read)



--word1 boundary word2 boundary word3
--syll11 syll12 boundary syll21 syll22 syll23 boundary syll31 syll32
--phoneme1n boundary phoneme21n phoneme22n boundary phoneme3n
