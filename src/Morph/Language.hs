module Morph.Language
( morphLanguage
) where

import ClassyPrelude
import Data.RVar
import Data.Random.Extras

import Data.Language
import Data.Inflection

import Morph.Phonology
import Morph.Grammar

import Out.Roman


morphLanguage :: Language -> RVar Language
morphLanguage parent = do
  langN <- join $ choice [ phonologicalChange 0 parent
                         ]
  grammarN <- morphGrammar (getGrammar parent)

  -- find out what was assigned to "!!!LANGUAGE!!!" and romanize new language name
  let langNameN = fromMaybe "!!!NAME NOT FOUND!!!" (romanizeSyllWord . snd <$> find (\x -> fst x == ("!!!LANGUAGE!!!", Noun)) (getRoots langN))

  return $ langN{getName = langNameN, getGrammar=grammarN}

-- add or delete consonant clusters
morphPhonotactics :: Language -> RVar Language
morphPhonotactics parent = return parent

-- change grammatical categories around
morphInflection :: Language -> RVar Language
morphInflection parent = return parent

-- regenerate a few words/roots
morphLexicon :: Language -> RVar Language
morphLexicon parent = return parent

-- probably just regenerate a few characters
morphWritingSystem :: Language -> RVar Language
morphWritingSystem parent = return parent
