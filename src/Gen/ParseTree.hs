{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
module Gen.ParseTree
( makeParseTree
) where

import ClassyPrelude

import Data.RVar
import Data.Random.Extras

import Data.Grammar
import Data.Inflection
import Data.Phoneme

-- make parse tree using new language's root dictionary and inflection/conjugation systems
makeParseTree :: [((Text, LexCat), Morpheme)] -> InflectionMap -> RVar Phrase
makeParseTree dict inflmap = do

  let noun = fst.fst <$> choice (filter (\x -> snd (fst x) == Noun) dict)
  let prep = fst.fst <$> choice (filter (\x -> snd (fst x) == Adpo) dict)
  let verb = fst.fst <$> choice (filter (\x -> snd (fst x) == Verb) dict)
  let adj = fst.fst <$> choice (filter (\x -> snd (fst x) == Adj) dict)


  let objDet = generateInflection inflmap Noun
  let subjDet = generateInflection inflmap Noun
  let vInfl = generateInflection inflmap Verb

  let adjp = XP Adj Null XPNull <$> (XBarC Adj Null <$> (Leaf Adj Null <$> adj) <*> return XPNull)

  let subjp = XP Det Null
                XPNull <$>
                (XBarC Det Null <$>
                  (LeafInfl Noun <$>
                    subjDet) <*>
                  (XP Noun Null
                    XPNull <$>
                    (XBarA Noun Null <$>
                      adjp <*>
                      (XBarC Noun Null <$>
                       (Leaf Noun Null <$>
                          noun) <*>
                        return XPNull))))

  let objp = XP Det Null
               XPNull <$>
               (XBarC Det Null <$>
                 (LeafInfl Noun <$>
                   objDet) <*>
                 (XP Noun Null
                   XPNull <$>
                   (XBarA Noun Null <$>
                     adjp <*>
                     (XBarC Noun Null <$>
                       (Leaf Noun Null <$>
                         noun) <*>
                       return XPNull))))

  let prepp = XP Adpo Null XPNull <$> (XBarC Adpo Null <$> (Leaf Adpo Null <$> prep) <*> objp)

  -- output
  XP Comp Null
    XPNull <$>
    (XBarC Comp Null
     (LeafNull Null) <$>
     (XP Infl Null
       XPNull <$>
       (XBarC Infl Null <$>
         (LeafInfl Verb <$> vInfl) <*>
         (XP Verb Null <$>
           subjp <*>
           (XBarC Verb Null <$>
             (Leaf Verb Null <$>
               verb) <*>
             prepp)))))

-- gen verb Inflection
generateInflection :: InflectionMap -> LexCat -> RVar AllExpress
generateInflection inflMap lc = do
  gen <- helperFunction inflMap getGenSys lc
  ani <- helperFunction inflMap getAniSys lc
  cas <- helperFunction inflMap getCasSys lc
  num <- helperFunction inflMap getNumSys lc
  def <- helperFunction inflMap getDefSys lc
  spe <- helperFunction inflMap getSpeSys lc
  top <- helperFunction inflMap getTopSys lc
  per <- helperFunction inflMap getPerSys lc
  hon <- helperFunction inflMap getHonSys lc
  pol <- helperFunction inflMap getPolSys lc
  ten <- helperFunction inflMap getTenSys lc
  asp <- helperFunction inflMap getAspSys lc
  moo <- helperFunction inflMap getMooSys lc
  voi <- helperFunction inflMap getVoiSys lc
  evi <- helperFunction inflMap getEviSys lc
  tra <- helperFunction inflMap getTraSys lc
  vol <- helperFunction inflMap getVolSys lc

  return (gen, ani, cas, num, def, spe, top, per, hon, pol, ten, asp, moo, voi, evi, tra, vol)

helperFunction :: Eq a => InflectionMap -> (InflectionMap -> Manifest a) -> LexCat -> RVar (Express a)
helperFunction inflMap f lc = out where
  fMan = f inflMap
  out
    | fMan == NoManifest = return NoExpress
    | any (\(x,_,_) -> x == lc) (getManPlace fMan) = Express <$> choice (getManStuff fMan)
    | otherwise = return NoExpress


-- old version of makeParseTree
{-
makeParseTree2 :: MeaningData -> RVar Phrase
makeParseTree2 mData = out where

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
-}
