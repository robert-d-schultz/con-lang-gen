module GrammarGen
( makeGrammarSystem
, loadInputData2
) where

import Prelude hiding (Word)
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Control.Monad

import PhonemeInventoryGen
import PhonemeType
import PhonotacticsGen
import OtherData
import GrammarType

-- Generates a framework from which declension can be made

data InputData = InputData
    {
      inputGender       :: [[Gender]]
    , inputAnimacy      :: [[Animacy]]
    , inputCase         :: [[Case]]
    , inputNumber       :: [[Number]]
    , inputHonorific    :: [[Honorific]]
    , inputDefiniteness :: [[Definiteness]]
    , inputSpecificity  :: [[Specificity]]
    }

loadInputData2 :: IO InputData
loadInputData2 =
    InputData
        <$> readFeature "raw/grammatical categories/declension/gender.txt"
        <*> readFeature "raw/grammatical categories/declension/animacy.txt"
        <*> readFeature "raw/grammatical categories/declension/case.txt"
        <*> readFeature "raw/grammatical categories/declension/number.txt"
        <*> readFeature "raw/grammatical categories/declension/honorific.txt"
        <*> readFeature "raw/grammatical categories/declension/definiteness.txt"
        <*> readFeature "raw/grammatical categories/declension/specificity.txt"

readFeature :: Read a => FilePath -> IO a
readFeature = fmap read . readFile

-- create "grammar system"
-- all this does so far is picks between predefined systems
makeGrammarSystem :: InputData -> RVar GrammarSystem
makeGrammarSystem inputD = GrammarSystem
    <$> makeGenderSystem inputD
    <*> makeAnimacySystem inputD
    <*> makeCaseSystem inputD
    <*> makeNumberSystem inputD
    <*> makeHonorificSystem inputD
    <*> makeDefinitenessSystem inputD
    <*> makeSpecificitySystem inputD

makeGenderSystem :: InputData -> RVar (Maybe [Gender])
makeGenderSystem inputD = do
  a <- choice $ inputGender inputD
  choice [Nothing]--, Just a]

makeAnimacySystem :: InputData -> RVar (Maybe [Animacy])
makeAnimacySystem inputD = do
  a <- choice $ inputAnimacy inputD
  choice [Nothing]--, Just a]

makeCaseSystem :: InputData -> RVar (Maybe [Case])
makeCaseSystem inputD = do
  a <- choice $ inputCase inputD
  choice [Nothing]--, Just a]

makeNumberSystem :: InputData -> RVar (Maybe [Number])
makeNumberSystem inputD = do
  a <- choice $ inputNumber inputD
  choice [Nothing]--, Just a]

makeHonorificSystem :: InputData -> RVar (Maybe [Honorific])
makeHonorificSystem inputD = do
  a <- choice $ inputHonorific inputD
  choice [Nothing]--, Just a]

makeDefinitenessSystem :: InputData -> RVar (Maybe [Definiteness])
makeDefinitenessSystem inputD = do
  a <- choice $ inputDefiniteness inputD
  choice [Just a]

makeSpecificitySystem ::  InputData -> RVar (Maybe [Specificity])
makeSpecificitySystem inputD = do
  a <- choice $ inputSpecificity inputD
  choice [Just a]
