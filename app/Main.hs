module Main where

--import PhonologyGen
import Data.Random.Extras
import Data.Random hiding (sample)
import Data.RVar
import PhonemeInventoryGen
import PhonotacticsGen
import WordGen
import Parse
import GrammarGen
import DeclensionGen

main :: IO ()
main = do
  inventoryC <- sampleRVar (makeConInventory 21)
  inventoryV <- sampleRVar (makeVowInventory 5)
  phonotacticC <- sampleRVar (makeConPhonotactics inventoryC)
  phonotacticV <- sampleRVar (makeVowPhonotactics phonotacticC inventoryV)
  let syllList = makeAllSyllables phonotacticC phonotacticV
  wrds <- sampleRVar (makeDictionary 50 syllList)
  ldata <- loadInputData2
  grammarSys <- sampleRVar (makeGrammarSystem ldata)
  declension <- sampleRVar (makeDeclension grammarSys syllList inventoryC inventoryV)

  writeFile "dictionary.txt" $ parseConPhonemeInventory inventoryC
                                 ++ parseVowPhonemeInventory inventoryV
                                 ++ parseConPhonotactics phonotacticC
                                 ++ parseVowPhonotactics phonotacticV
                                 ++ show grammarSys ++ "\n"
                                 ++ show declension ++ "\n"
                                 ++ parseDictionary wrds
