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
  inventoryC <- sampleRVar (makeConInventory 12)
  inventoryV <- sampleRVar (makeVowInventory 4)
  phonotacticC <- sampleRVar (makeConPhonotactics inventoryC)
  phonotacticV <- sampleRVar (makeVowPhonotactics phonotacticC inventoryV)
  let syllList = makeAllSyllables phonotacticC phonotacticV
  wrds <- sampleRVar (makeDictionary 50 syllList)
  ldata <- loadInputData2
  grammarSys <- sampleRVar (makeGrammarSystem ldata)
  (declensionSeed, declensionTemplate) <- sampleRVar (makeSeedTemplate grammarSys inventoryC inventoryV syllList)
  --let declension = (makeTriDeclension grammarSys declensionSeed declensionTemplate)

  writeFile "dictionary.txt" $ parseConPhonemeInventory inventoryC
                                 ++ parseVowPhonemeInventory inventoryV
                                 ++ parseConPhonotactics phonotacticC
                                 ++ parseVowPhonotactics phonotacticV
                                 ++ show grammarSys ++ "\n"
                                 ++ show declensionSeed ++ "\n"
                                 ++ show declensionTemplate ++ "\n"
                                 ++ show (numTemplate grammarSys declensionTemplate) ++ "\n"
                                 -- ++ show declension ++ "\n"
                                 ++ parseDictionary wrds
