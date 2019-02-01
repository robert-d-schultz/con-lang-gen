{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module LoadStuff
( MeaningData(..)
, loadMeaningData
, InputData(..)
, loadInputData
) where

import ClassyPrelude
import Prelude (read)

import Data.Inflection
import Data.Word

-- Input data for lexicon
data MeaningData = MeaningData
  {
      inputNouns  :: [Text]
    , inputVerbs  :: [Text]
    , inputAdjs   :: [Text]
    , inputAdpos  :: [Text]
    , inputDerivs :: [Meaning]
  }

loadMeaningData :: IO MeaningData
loadMeaningData =  MeaningData
  <$> readMeaning "raw/meanings/nouns.txt"
  <*> readMeaning "raw/meanings/verbs.txt"
  <*> readMeaning "raw/meanings/adjectives.txt"
  <*> readMeaning "raw/meanings/adpositions.txt"
  <*> return deriv


deriv = [ (DeriMeaning Verb Verb "To ask to ")
      , (DeriMeaning Verb Verb "The reverse action of ")
      , (DeriMeaning Noun Noun "A place with lots of ")
      , (DeriMeaning Noun Noun "A large ")
      , (DeriMeaning Noun Noun "A small or cute ")
      , (DeriMeaning Noun Noun "An undesirable ")
      , (DeriMeaning Noun Noun "Thing made from ")
      , (DeriMeaning Adj Adj "The opposite of ")
      , (DeriMeaning Verb Noun "A person who habitually ")
      , (DeriMeaning Verb Noun "A person who professionally ")
      , (DeriMeaning Verb Noun "The typical place to ")
      , (DeriMeaning Verb Noun "The typical time to ")
      , (DeriMeaning Verb Noun "A tool used to ")
      , (DeriMeaning Verb Noun "A substance used to ")
      , (DeriMeaning Verb Noun "A thing or substance to ")
      , (DeriMeaning Verb Noun "A thing or substance resulting from ")
      , (DeriMeaning Verb Noun "A style or method of doing ")
      , (DeriMeaning Verb Adj "Tending to often ")
      , (DeriMeaning Noun Verb "The way to typically use ")
      , (DeriMeaning Noun Verb "To seek ")
      , (DeriMeaning Noun Adj "Pertaining to ")
      , (DeriMeaning Noun Adj "Made of ")
      , (DeriMeaning Noun Adj "Resembling ")
      , (DeriMeaning Noun Adj "Supplied with ")
      , (DeriMeaning Noun Adj "Lacking ")
      , (DeriMeaning Adj Verb "Cause to become ")
      , (DeriMeaning Adj Noun "Something that is ")
      ]


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
readFeature x = read <$> unpack <$> decodeUtf8 <$> readFile x
