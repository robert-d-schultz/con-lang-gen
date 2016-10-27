module OtherData
( SyllWord(..)
, Syllable(..)
) where

import PhonemeData

-- Used to parse out syllables from a word
data SyllWord = SyllWord [Syllable] deriving (Eq, Show, Read)
data Syllable = Syllable
              { getOnset :: [Phoneme]
              , getNucleus :: Phoneme
              , getCoda :: [Phoneme]} deriving (Eq, Show, Read)
