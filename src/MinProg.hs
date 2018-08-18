module MinProg
(
) where

import Prelude
import Data.RVar
import Data.Random.Extras
import Data.List
import Data.Maybe

data Feature = N | V | UN | UV deriving (Show, Eq)

data SyntaxObj = LexItem { feats :: [Feature]
                         , str :: String
                         }
               | Branch { feats :: [Feature]
                        , left :: SyntaxObj
                        , right :: SyntaxObj
                        } deriving (Show, Eq)

type Numeration = [SyntaxObj]

data Derivation a = Crash a | Converge a deriving (Show)

derive :: Numeration -> SyntaxObj -> RVar (Derivation SyntaxObj)
derive num synObj = do
  foo <- selectForMerge num synObj
  bar <- selectForMove (subComps synObj) synObj
  let out | isJust foo = derive (fst $ fromJust foo) (merge synObj (snd $ fromJust foo))
          | isJust bar = derive num (move synObj (fromJust bar))
          | null (feats synObj) && null num = return $ Converge synObj
          | otherwise = return $ Crash synObj
  out

selectForMerge :: Numeration -> SyntaxObj -> RVar (Maybe (Numeration, SyntaxObj))
selectForMerge num synObj = do
  let filt = filter (selectFilter synObj) num
  ch <- sequence $ safeChoice filt
  let newNum = delete <$> ch <*> Just num
  return $ (,) <$> newNum <*> ch

selectForMove :: [SyntaxObj] -> SyntaxObj -> RVar (Maybe SyntaxObj)
selectForMove synObjs synObj = do
  let filt = filter (selectFilter synObj) synObjs
  sequence $ safeChoice filt

selectFilter :: SyntaxObj -> SyntaxObj -> Bool
selectFilter synObj sub = out where
  fs1 = feats synObj
  fs2 = feats sub
  out = or [ last fs1 == UV && V `elem` fs2
           , last fs2 == UV && V `elem` fs1
           , last fs1 == UN && N `elem` fs2
           , last fs2 == UN && N `elem` fs1
           ]

merge :: SyntaxObj -> SyntaxObj -> SyntaxObj
merge synObj newSynObj = Branch newFeats newSynObj synObj where
  newFeats = feats synObj -- wrong, need "annihilation" between the two feat sets

move :: SyntaxObj -> SyntaxObj -> SyntaxObj
move synObj newSynObj = Branch newFeats newSynObj synObj where
  newFeats = feats synObj -- still wrong


subComps :: SyntaxObj -> [SyntaxObj]
subComps LexItem{} = []
subComps synObj = synObj : subComps (left synObj) ++ subComps (right synObj)

spellout :: SyntaxObj -> String
spellout (Branch _ l r) = spellout l ++ " " ++ spellout r
spellout (LexItem _ s) = s
