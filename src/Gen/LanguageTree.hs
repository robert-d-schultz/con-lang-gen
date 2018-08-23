{-# OPTIONS_GHC -Wall #-}
module Gen.LanguageTree
( makeLanguageTree
) where

import Data.Random.Extras hiding (sample)
import Data.Random
import Control.Monad

import LoadStuff

import Morph.Language

import Data.Other

import Gen.Language

-- makes a language tree
makeLanguageTree :: Int -> InputData -> MeaningData -> Language -> RVar LanguageBranch
makeLanguageTree iter idata mData RootL = do
  lang <- makeLanguage idata mData
  makeLanguageTree_ iter idata mData lang
makeLanguageTree iter idata mData parent = do
  lang <- morphLanguage parent
  makeLanguageTree_ iter idata mData lang

makeLanguageTree_ :: Int -> InputData -> MeaningData -> Language -> RVar LanguageBranch
makeLanguageTree_ iter idata mData lang = do

  -- end branching after threshold
  -- less branching early on
  let weight | iter > 4 = [0]
             | iter < 2 = [1,1,2]
             | otherwise = [1,1,2] -- [0,0,0,1,1,1,1,2,3,4]

  child_n <- choice weight

  children <- replicateM child_n (makeLanguageTree (iter + 1) idata mData lang)

  -- (un)used for branch length
  n <- uniform 10 10
  return $ LanguageBranch lang children n
