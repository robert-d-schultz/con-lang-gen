module Gen.LanguageTree
( makeLanguageTree
) where

import Data.Random.Extras hiding (sample)
import Data.Random
import Data.RVar
import Data.List
import Control.Monad

import LoadStuff

import Morph.Language

import Data.Other

-- identity
makeLanguageTree :: Int -> InputData -> MeaningData -> Language -> RVar LanguageBranch
makeLanguageTree iter idata mData parent = do
  lang <- morphLanguage parent
  let weight | iter > 4 = [0]
             | iter < 2 = [1,1,2]
             | otherwise = [0,0,0,1,1,1,1,2,3,4]

  child_n <- choice weight

  children <- replicateM child_n (makeLanguageTree (iter + 1) idata mData lang)

  n <- uniform 10 10
  return $ LanguageBranch lang children n
