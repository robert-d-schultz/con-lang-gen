module Data.Other
( SyllWord(..)
, Syllable(..)
, Language(..)
, LanguageBranch(..)
) where

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
              { getName :: String
              , getCs :: [Phoneme]
              , getVs :: [Phoneme]
              , getDs :: [Phoneme]
              , getSonHier :: [[Phoneme]]
              , getOnsetCCs :: [[Phoneme]]
              , getCodaCCs :: [[Phoneme]]
              , getInflSys :: InflectionSystem
              , getInflSyss :: [(LexCat, [ManifestSystem], [ManifestSystem], [ManifestSystem])]
              , getGrammar :: Grammar
              , getRoots :: [((String, LexCat), Morpheme)]
              , getWriting :: ([(Phoneme, (Int, [(String,[(Int,Int)])]))], [(Syllable, (Int, [(String,[(Int,Int)])]))], [(((String, LexCat), Morpheme), (Int, [(String,[(Int,Int)])]))])
              } deriving (Show)

-- Used to parse out syllables from a word
newtype SyllWord = SyllWord [Syllable] deriving (Eq, Show, Read)
data Syllable = Syllable
              { getOnset :: [Phoneme]
              , getNucleus :: Phoneme
              , getCoda :: [Phoneme]
              } deriving (Eq, Show, Read)
