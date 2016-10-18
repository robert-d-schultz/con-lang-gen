module Main where

--import PhonologyGen
import Data.Random.Extras
import Data.Random hiding (sample)
import Data.RVar
import PhonemeInventoryGen2
import PhonotacticsGen2
import WordGen2
import Parse2

main :: IO ()
main = do
  -- inventoryC <- sampleRVar (makeConInventory 12)
  places <- sampleRVar makePlaces
  manners <- sampleRVar makeManners
  phonations <- sampleRVar makePhonations
  inventoryC <- sampleRVar (makeConsonants places manners phonations)
  inventoryV <- sampleRVar (makeVowInventory 4)
  inventoryD <- sampleRVar (makeDiphInventory 4 inventoryV)
  sonHier <- sampleRVar (makeSonHier inventoryC)
  wrds <- sampleRVar (makeDictionary 50 (inventoryV ++ inventoryD) sonHier ((1, 4), (0, 2), (1, 3), (0, 2)))
  let syllWrds = map (syllabifyWord sonHier) wrds
  writeFile "dictionary.txt" $ parseConPhonemeInventory inventoryC
                                 ++ show inventoryC ++ "\n"
                                 ++ parseVowPhonemeInventory inventoryV
                                 ++ parseDiphPhonemeInventory inventoryD
                                 ++ parseSonHier (inventoryV ++ inventoryD) sonHier
                                -- ++ show (head wrds) ++ "\n"
                                 ++ parseDictionary syllWrds
