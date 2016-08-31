module Main where

import PhonologyGen
import qualified Data.Random.Sample as Sampler

main :: IO ()
main = do
  inventory <- Sampler.sample (makeConsonantPhonemeInventory 5)
  print inventory
