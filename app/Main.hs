module Main where

--import PhonologyGen
import Data.Random.Extras
import Data.Random hiding (sample)
import Data.RVar

import PhonemeGen
import PhonotacticsGen
import WordGen
import Parse
import InflectionGen
import MorphologyGen

main :: IO ()
main = do
  -- consonants
  places <- sampleRVar makePlaces
  manners <- sampleRVar makeManners
  phonations <- sampleRVar makePhonations
  inventoryC <- sampleRVar (makeConsonants places manners phonations)

  -- vowels
  heights <- sampleRVar makeHeights
  backs <- sampleRVar makeBacknesses
  rounds <- sampleRVar makeRoundedneses
  lengths <- sampleRVar makeLengths
  inventoryV <- sampleRVar (makeVowels heights backs rounds lengths)

  -- diphthongs
  inventoryD <- sampleRVar (makeDiphInventory 4 inventoryV)

  -- phonotactics
  sonHier <- sampleRVar (makeSonHier inventoryC)

  -- root morphemes
  roots <- sampleRVar (makeDictionary 50 (inventoryV ++ inventoryD) sonHier ((1, 4), (0, 2), (1, 3), (0, 2)))

  -- inflection / grammatical categories
  idata <- loadInputData
  (inflSys, shit) <- sampleRVar (makeInflectionSystem idata)
  systems <- sampleRVar (mapM (makeLexicalInflection inventoryV sonHier inflSys) shit)

  -- outputs
  writeFile "phonology.txt" $ "Phonology"
                            ++ parseConPhonemeInventory places manners phonations inventoryC
                            ++ parseVowPhonemeInventory heights backs rounds lengths inventoryV
                            ++ parseDiphPhonemeInventory inventoryD

  writeFile "phonotactics.txt" $ "Phonotactics"
                               ++ parseSonHier (inventoryV ++ inventoryD) sonHier

  writeFile "inflection.txt" $ "Inflection"
                            -- ++ show inflSys ++ "\n"
                            -- ++ show shit ++ "\n"
                            -- ++ show systems ++ "\n"
                             ++ parseLCInflection inflSys
                             ++ concatMap (parseLexicalSystems inflSys) systems

  writeFile "lexicon.txt" $ "Lexicon"
                         ++ parseDictionary sonHier roots
