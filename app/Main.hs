module Main where

--import PhonologyGen
import Data.Random.Extras
import Data.Random hiding (sample)
import Data.RVar
import PhonemeInventoryGen2
import PhonotacticsGen2
import WordGen2
import Parse2
import GrammarGen2
import MorphologyGen

main :: IO ()
main = do
  -- inventoryC <- sampleRVar (makeConInventory 12)
  places <- sampleRVar makePlaces
  manners <- sampleRVar makeManners
  phonations <- sampleRVar makePhonations
  inventoryC <- sampleRVar (makeConsonants places manners phonations)
  heights <- sampleRVar makeHeights
  backs <- sampleRVar makeBacknesses
  rounds <- sampleRVar makeRoundedneses
  lengths <- sampleRVar makeLengths
  inventoryV <- sampleRVar (makeVowels heights backs rounds lengths)
  inventoryD <- sampleRVar (makeDiphInventory 4 inventoryV)
  sonHier <- sampleRVar (makeSonHier inventoryC)
  wrds <- sampleRVar (makeDictionary 50 (inventoryV ++ inventoryD) sonHier ((1, 4), (0, 2), (1, 3), (0, 2)))
  let syllWrds = map (syllabifyWord sonHier) wrds
  idata <- loadInputData2
  (gramSys, (nPar, nExp)) <- sampleRVar (makeGrammarSystem idata)
  expSystems <- sampleRVar (makeExponentSystems nExp inventoryV sonHier gramSys)
  parSystems <- sampleRVar (makeParticleSystems nPar inventoryV sonHier gramSys)

  writeFile "phonology.txt" $ "Phonology"
                            ++ parseConPhonemeInventory places manners phonations inventoryC
                            ++ parseVowPhonemeInventory heights backs rounds lengths inventoryV
                            ++ parseDiphPhonemeInventory inventoryD

  writeFile "phonotactics.txt" $ "Phonotactics"
                               ++ parseSonHier (inventoryV ++ inventoryD) sonHier

  writeFile "grammar.txt" $ "Grammar"
                      --   ++ show gramSys ++ "\n"
                         ++ parseExponentSystems (length expSystems) expSystems gramSys
                      --   ++ show particleSystems ++ "\n"
                      --   ++ parseGrammarSystem gramSys

  writeFile "lexicon.txt" $ "Lexicon"
                         ++ parseDictionary syllWrds
