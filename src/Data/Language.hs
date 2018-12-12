module Data.Language
( Language(..)
, LanguageBranch(..)
) where

import ClassyPrelude

import Data.Phoneme
import Data.Word
import Data.Grammar
import Data.Inflection
import Data.Soundchange
import Data.Other

-- Language trees
data LanguageBranch = LanguageBranch
                    { getLanguage :: Language
                    , getChildren :: [LanguageBranch]
                    , getN :: Int
                    }
-- Language
data Language = Language
              { getName :: Text
              , getNameMod :: (Text, Text)
              , getCInv :: [Phoneme]
              , getVInv :: [Phoneme]
              , getDInv :: [Phoneme]
              , getCMap :: ([Place], [Manner], [Phonation], [Airstream])
              , getVMap :: ([Height], [Backness], [Roundedness], [Length])
              , getTones :: [Tone]
              , getSonHier :: Int
              , getOnsetCCs :: [ConsCluster]
              , getNuclei :: [Phoneme]
              , getCodaCCs :: [ConsCluster]
              , getInflMap :: InflectionMap
              , getInflMorphemes :: [Morpheme]
              , getLemmaMorphemes :: [Morpheme]
              , getRootMorphemes :: [Morpheme]
              , getGrammar :: Grammar
              , getWriting :: ([(Phoneme, (Int, CharPath))], [(Syllable, (Int, CharPath))], [(Morpheme, (Int, CharPath))])
              , getRules :: [Rule]
              } deriving (Show)
