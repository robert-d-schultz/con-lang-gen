module OtherData2
( Word(..)
, SyllWord(..)
, Syllable(..)
) where

import Prelude hiding (Word)
import PhonemeType2

data Word = Word [Phoneme] deriving (Eq, Show, Read)
data SyllWord = SyllWord [Syllable] deriving (Eq, Show, Read)
data Syllable = Syllable
              { getOnset :: [Phoneme]
              , getNucleus :: Phoneme
              , getCoda :: [Phoneme]} deriving (Eq, Show, Read)
