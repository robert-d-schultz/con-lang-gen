module Main where

import ClassyPrelude

import Data.Random
import System.Random
import System.Directory

import LoadStuff

import Data.Language

import Gen.LanguageTree

import Out.Language

main :: IO ()
main = do
  -- Seed
  input <- putStr "Enter seed: " *> getLine
  let seed = hashWithSalt 1 input
  setStdGen $ mkStdGen seed

  idata <- loadInputData
  mData <- loadMeaningData
  tree <- newSample $ makeLanguageTree idata mData
  let name = getName $ getLanguage tree
  exist <- doesPathExist $ "out/" ++ unpack name ++ " language family"

  if exist then putStrLn "Language family already generated" *> main else
    writeLanguageTree seed tree

-- Special sampleRVar that allows seeds
newSample :: RVar a -> IO a
newSample i = do
  g1 <- getStdGen
  let out = sampleState i g1
  setStdGen $ snd out
  return $ fst out
