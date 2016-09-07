module Main where

--import PhonologyGen
import XBar
import Data.Random
import Data.RVar


main :: IO ()
main = do
--  inventory <- Sampler.sample (makeConsonantPhonemeInventory 5)
  dat <- loadInputData
  tree <- sampleRVar (makeTenseP dat 6)
--  print $ parseTenseP tree
  print tree
