{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Gen.ParseTree
( makeParseTree
) where

import ClassyPrelude

import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)

import LoadStuff
import Data.Grammar
import Data.Inflection

makeParseTree :: MeaningData -> RVar Phrase
makeParseTree mData = out where

  noun = choice $ inputNouns mData
  prep = choice $ inputAdpos mData
  verb = choice $ inputVerbs mData
  adj  = choice $ inputAdjs mData

  vInfl = (NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress)
  subjDet = (NoExpress, NoExpress, Express NOM, Express SG, Express DEF, NoExpress, Express TOP, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress)
  objDet = (NoExpress, NoExpress, Express ACC, Express SG, Express DEF, NoExpress, Express NTOP, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress)

  adjp = join $ choice [return XPNull, XP Adj Null XPNull <$> (XBarC Adj Null <$> (Leaf Adj Null <$> adj) <*> return XPNull)]
  prepp = join $ choice [return XPNull, return XPNull, return XPNull, XP Adpo Null XPNull <$> (XBarC Adpo Null <$> (Leaf Adpo Null <$> prep) <*> objp)]
  prepp2 = XP Adpo Null XPNull <$> (XBarC Adpo Null <$> (Leaf Adpo Null <$> prep) <*> objp)

  objp = XP Det Null XPNull <$> (XBarC Det Null (LeafInfl Noun objDet) <$> (XP Noun Null XPNull <$> (XBarA Noun Null <$> adjp <*> (XBarC Noun Null <$> (Leaf Noun Null <$> noun) <*> prepp))))
  subjp = XP Det Null XPNull <$> (XBarC Det Null (LeafInfl Noun subjDet) <$> (XP Noun Null XPNull <$> (XBarA Noun Null <$> adjp <*> (XBarC Noun Null <$> (Leaf Noun Null <$> noun) <*> prepp))))

  obj = join $ choice [objp, prepp2]

  out = XP Comp Null XPNull <$> (XBarC Comp Null (LeafNull Null) <$> (XP Infl Null XPNull <$> (XBarC Infl Null (LeafInfl Verb vInfl) <$> (XP Verb Null <$> subjp <*> (XBarC Verb Null <$> (Leaf Verb Null <$> verb) <*> obj)))))
