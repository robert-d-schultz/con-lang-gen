-- Data types for allophones (potentially anything though)
module PhonotacticType
(
) where

import Prelude
import PhonemeType

data SyllType = CVC | CV | VC | V deriving (Eq, Show, Read, Enum, Bounded, Ordering)

data SyllRules = CVC
                { onsetC :: [Consonant]
                , nucleusV :: [Vowel]
                , codaC :: [Consonant]
                }
              | CV
                { onsetC :: [Consonant]
                , codaV :: [Vowel]
                }
              | VC
                { onsetV :: [Vowel]
                , codaC :: [Consonant]
                }
              | V
                { nucleusV :: [Vowel]
                }  deriving (Eq, Show, Read)


data ConCluster = ConCluster [String] deriving (Eq, Show, Read)
