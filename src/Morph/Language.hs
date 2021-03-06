module Morph.Language
( morphLanguage
) where

import ClassyPrelude
import Data.RVar
import Data.Random.Extras

import Data.Language
import Data.Word
import Data.Inflection

import Morph.Phonology
import Morph.Grammar

import Out.Roman
import Out.Lexicon (applyLemma)


morphLanguage :: Language -> RVar Language
morphLanguage parent = do
  langN <- join $ choice [ phonologicalChange 0 parent
                         ]
  grammarN <- morphGrammar (getGrammar parent)

  let lemmaMorphsN = getLemmaMorphemes langN

  -- Find out what was assigned to "!!!LANGUAGE!!!" and romanize new language name
  let nameRootN = find (\x -> getMeaning x == RootMeaning Noun "!!!LANGUAGE!!!") (getRootMorphemes langN)
  let langNameN = fromMaybe "!!!name not found!!!" (romanizeWord langN <$> join (applyLemma lemmaMorphsN <$> nameRootN))

  return $ langN{getName = langNameN, getGrammar=grammarN}

-- Add or delete consonant clusters
morphPhonotactics :: Language -> RVar Language
morphPhonotactics parent = return parent

-- Change grammatical categories around
morphInflection :: Language -> RVar Language
morphInflection parent = return parent

-- Re-Generate a few words/roots
morphLexicon :: Language -> RVar Language
morphLexicon parent = return parent

-- Probably just regenerate a few characters
morphWritingSystem :: Language -> RVar Language
morphWritingSystem parent = return parent
