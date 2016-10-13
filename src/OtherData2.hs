module OtherData2
( Word(..)
, SyllWord(..)
, Syllable(..)
) where

import Prelude hiding (Word)
import PhonemeType2

data Word = Word [Phone] deriving (Eq, Show, Read)
data SyllWord = SyllWord [Syllable] deriving (Eq, Show, Read)
data Syllable = Syllable
              { getOnset :: [Phone]
              , getNucleus :: Phone
              , getCoda :: [Phone]} deriving (Eq, Show, Read)
