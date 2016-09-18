module Main where

--import PhonologyGen
import Data.Random
import Data.RVar
import PhonemeInventoryGen
import PhonotacticsGen


main :: IO ()
main = do
  inventoryc <- sampleRVar (makeNaiveConInventory 10)
  inventoryv <- sampleRVar (makeNaiveVowInventory 5)
  ctact <- sampleRVar (splitC inventoryc)
  vtact <- sampleRVar (splitV ctact inventoryv)
  putStr $ show ctact
  putStr "\n"
  putStr $ show vtact
