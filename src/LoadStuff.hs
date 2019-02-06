{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module LoadStuff
( MeaningData(..)
, loadMeaningData
, InputData(..)
, loadInputData
, readMeaning
) where

import ClassyPrelude
import Prelude (read)

import Data.Inflection
import Data.Word

-- Input data for lexicon
data MeaningData = MeaningData
  {
      inputRoots  :: [Meaning]
    , inputDerivs :: [Meaning]
    , inputCompounds :: [Meaning]
  }

loadMeaningData :: IO MeaningData
loadMeaningData =  MeaningData
  <$> concat <$> sequence [ readMeaning "raw/meanings/nouns.txt"
                          , readMeaning "raw/meanings/verbs.txt"
                          , readMeaning "raw/meanings/adjectives.txt"
                          , readMeaning "raw/meanings/adpositions.txt"
                          ]
  <*> readMeaning "raw/meanings/derivation.txt"
  <*> readMeaning "raw/meanings/compound.txt"

readMeaning :: Read a => FilePath -> IO a
readMeaning x = read <$> unpack <$> readFileUtf8 x

-- Input data for inflections/morphology
data InputData = InputData
    {
      inputGender        :: [[Gender]]
    , inputAnimacy       :: [[Animacy]]
    , inputCase          :: [[Case]]
    , inputNumber        :: [[Number]]
    , inputDefiniteness  :: [[Definiteness]]
    , inputSpecificity   :: [[Specificity]]
    , inputTopic         :: [[Topic]]
    , inputPerson        :: [[Person]]
    , inputHonorific     :: [[Honorific]]
    , inputPolarity      :: [[Polarity]]
    , inputTense         :: [[Tense]]
    , inputAspect        :: [[Aspect]]
    , inputMood          :: [[Mood]]
    , inputVoice         :: [[Voice]]
    , inputEvidentiality :: [[Evidentiality]]
    , inputTransitivity  :: [[Transitivity]]
    , inputVolition      :: [[Volition]]
    }

loadInputData :: IO InputData
loadInputData =
    InputData
        <$> readFeature "raw/grammatical categories/gender.txt"
        <*> readFeature "raw/grammatical categories/animacy.txt"
        <*> readFeature "raw/grammatical categories/case.txt"
        <*> readFeature "raw/grammatical categories/number.txt"
        <*> readFeature "raw/grammatical categories/definiteness.txt"
        <*> readFeature "raw/grammatical categories/specificity.txt"
        <*> readFeature "raw/grammatical categories/topic.txt"
        <*> readFeature "raw/grammatical categories/person.txt"
        <*> readFeature "raw/grammatical categories/honorific.txt"
        <*> readFeature "raw/grammatical categories/polarity.txt"
        <*> readFeature "raw/grammatical categories/tense.txt"
        <*> readFeature "raw/grammatical categories/aspect.txt"
        <*> readFeature "raw/grammatical categories/mood.txt"
        <*> readFeature "raw/grammatical categories/voice.txt"
        <*> readFeature "raw/grammatical categories/evidentiality.txt"
        <*> readFeature "raw/grammatical categories/transitivity.txt"
        <*> readFeature "raw/grammatical categories/volition.txt"

readFeature :: Read a => FilePath -> IO a
readFeature x = read <$> unpack <$> readFileUtf8 x
