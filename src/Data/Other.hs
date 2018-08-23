{-# LANGUAGE NoImplicitPrelude #-}
module Data.Other
( SyllWord(..)
, Syllable(..)
, Language(..)
, LanguageBranch(..)
) where

import ClassyPrelude

import Data.Phoneme
import Data.Grammar
import Data.Inflection

-- Language trees
data LanguageBranch = LanguageBranch
                    { getLanguage :: Language
                    , getChildren :: [LanguageBranch]
                    , getN :: Int
                    }
-- Language
data Language = RootL
              | Language
              { getName :: Text
              , getCMap :: ([Place], [Manner], [Phonation])
              , getCInv :: [Phoneme]
              , getVMap :: ([Height], [Backness], [Roundedness], [Length], [Tone])
              , getVInv :: [Phoneme]
              , getDInv :: [Phoneme]
              , getSonHier :: Int
              , getOnsetCCs :: [[Phoneme]]
              , getNuclei :: [Phoneme]
              , getCodaCCs :: [[Phoneme]]
              , getInflMap :: InflectionMap
              , getManSyss :: [ManifestSystem]
              , getGrammar :: Grammar
              , getRoots :: [((Text, LexCat), Morpheme)]
              , getWriting :: ([(Phoneme, (Int, [(Text,[(Int,Int)])]))], [(Syllable, (Int, [(Text,[(Int,Int)])]))], [(((Text, LexCat), Morpheme), (Int, [(Text,[(Int,Int)])]))])
              } deriving (Show)

-- Used to parse out syllables from a word
newtype SyllWord = SyllWord [Syllable] deriving (Eq, Show, Read)
type ConsonantCluster = [Phoneme]
data Syllable = Syllable
              { getOnset :: [Phoneme]
              , getNucleus :: Phoneme
              , getCoda :: [Phoneme]
              } deriving (Eq, Show, Read)
