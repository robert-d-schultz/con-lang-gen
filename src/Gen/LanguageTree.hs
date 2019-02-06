module Gen.LanguageTree
( makeLanguageTree
) where

import ClassyPrelude

import Data.Random.Extras
import Data.Random hiding (sample)

import Data.Language
import Gen.Language
import Morph.Language

import LoadStuff

-- Makes a language tree
makeLanguageTree :: InputData -> MeaningData -> RVar LanguageBranch
makeLanguageTree idata mData = do
  lang <- makeLanguage idata mData -- initial language
  branches <- makeLanguageBranch 0 idata mData lang
  let tree = LanguageBranch lang [branches] 10
  assignModifiers tree

makeLanguageBranch :: Int -> InputData -> MeaningData -> Language -> RVar LanguageBranch
makeLanguageBranch iter idata mData lang = do

  -- End branching after threshold
  -- Less branching early on
  let weight | iter > 2 = [0]
             | otherwise = [1]
             -- | iter < 2 = [1,1,2]
             -- | otherwise = [0,0,0,1,1,1,1,2,3,4]

  childN <- choice weight

  parent <- morphLanguage lang

  children <- replicateM childN (makeLanguageBranch (iter+1) idata mData parent)

  -- (un)used for branch length
  n <- uniform 10 10
  return $ LanguageBranch parent children n


-- All of this doesn't quite work
-- Might need to seperate temporal and spacial modifiers in Data.Language
assignModifiers :: LanguageBranch -> RVar LanguageBranch
assignModifiers lb = do
  let depth = treeDepth lb
  assignModifiers2 0 depth lb

-- Traverse the Tree, disambiguating languages with disambiguation modifiers
-- i = current depth in tree
-- j = max depth of tree
assignModifiers2 :: Int -> Int -> LanguageBranch -> RVar LanguageBranch
assignModifiers2 i j lb@(LanguageBranch lang [] _) = return lb
assignModifiers2 i j lb@(LanguageBranch lang langs _) = do
  let (parentT, parentS) = getNameMod lang

  let (match, nomatch) = partition (\x -> getName lang == getName (getLanguage x)) langs

  let numMods | length match == 2 = join $ choice [ return ["Continental ", "Insular "]
                                                  , return ["Hill ", "Meadow "]
                                                  , return ["High ", "Low "]
                                                  , return ["Upper ", "Lower "]
                                                  , sample 2 ["East ", "West ", "North ", "South "]
                                                  , sample 2 ["Eastern ", "Western ", "Northern ", "Southern "]
                                                  ]
              | length match == 4 = choice [ ["East ", "West ", "North ", "South "]
                                           , ["Eastern ", "Western ", "Northern ", "Southern "]
                                           ]
              | length match == 5 = choice [ ["East ", "West ", "North ", "South ", "Central "]
                                           , ["Eastern ", "Western ", "Northern ", "Southern ", "Midland "]
                                           ]
              | otherwise = sample (length match) ["East ", "West ", "North ", "South ", "Central ", "Midland ", "High ", "Low ", "Continental ", "Insular "]

  sMods <- numMods

  let pickSMods | length match == 1 = [""]
                | otherwise = sMods

  let pickTMod | length match == 1 && null (getChildren (unsafeHead match)) && ((i+1) == j) = "Modern "
               | length match == 1 && null (getChildren (unsafeHead match)) = "Late "
               | length match == 1 = getTModifier (j-i-1)
               | otherwise = ""

  let matchN = zipWith (\x y -> x{getLanguage = (getLanguage x){getNameMod = (pickTMod, y ++ parentS ++ snd (getNameMod (getLanguage x)))}}) match pickSMods

  langsN <- mapM (assignModifiers2 (i+1) j) (matchN ++ nomatch)

  let langN = if length match == 1
            then lang{getNameMod = (getTModifier (j-i), snd (getNameMod lang))}
            else lang{getNameMod = ("", snd (getNameMod lang))}

  return lb{getLanguage = langN, getChildren = langsN}

getTModifier :: Int -> Text
getTModifier = unsafeIndex (["Middle ", "Old ", "Early ", "Classical ", "Archaic ", "Ancient ", "Primitive "] ++ map (\x -> "Period " ++ tshow x ++ " ") [1..])

treeDepth :: LanguageBranch -> Int
treeDepth lb@(LanguageBranch _ [] _) = 0
treeDepth lb@(LanguageBranch _ langs _) = fromMaybe 0 $ maximumMay $ map ((1+).treeDepth) langs
