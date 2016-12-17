module Data.Other
( SyllWord(..)
, Syllable(..)
) where

import Data.Phoneme

-- Used to parse out syllables from a word
data SyllWord = SyllWord [Syllable] deriving (Eq, Show, Read)
data Syllable = Syllable
              { getOnset :: [Phoneme]
              , getNucleus :: Phoneme
              , getCoda :: [Phoneme]} deriving (Eq, Show, Read)
