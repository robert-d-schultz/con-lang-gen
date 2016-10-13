module OtherData
( Word(..)
, Syllable(..)
) where

import Prelude hiding (Word)
import PhonemeType

data Word = Word [Syllable] deriving (Eq, Show, Read)
data Syllable = Syllable
              { getC1 :: MaybeConsonant
              , getV :: Vowel
              , getC2 :: MaybeConsonant} deriving (Eq, Show, Read)
