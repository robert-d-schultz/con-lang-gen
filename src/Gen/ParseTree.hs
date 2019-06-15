module Gen.ParseTree
( makeParseTree
, generateInflection
) where

import ClassyPrelude

import Data.RVar
import Data.Random.Extras

import Data.Word
import Data.Grammar
import Data.Inflection

-- Make parse tree using language's root dict and inflection/conjugation systems
makeParseTree :: [Morpheme] -> InflectionMap -> RVar Phrase
makeParseTree rootMorphs inflmap = do

  let noun = getStr.getMeaning <$> choice (filter (\x -> getLC (getMeaning x) == Noun) rootMorphs)
  let det  = getStr.getMeaning <$> choice (filter (\x -> getLC (getMeaning x) == Det) rootMorphs)
  let prep = getStr.getMeaning <$> choice (filter (\x -> getLC (getMeaning x) == Adpo) rootMorphs)
  let verb = getStr.getMeaning <$> choice (filter (\x -> getLC (getMeaning x) == Verb) rootMorphs)
  let adj  = getStr.getMeaning <$> choice (filter (\x -> getLC (getMeaning x) == Adj) rootMorphs)

  -- inflection phrases on Adj/Noun phrases?
  let objDet = generateInflection inflmap Noun
  let prepInfl = generateInflection inflmap Adpo
  let subjDet = generateInflection inflmap Noun
  let vInfl = generateInflection inflmap Verb
  let adjInfl = generateInflection inflmap Adj

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

-- Generate a random Inflection
generateInflection :: InflectionMap -> LexCat -> RVar GramCatExpress
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

  return $ GramCatExpress gen ani cas num def spe top per hon pol ten asp moo voi evi tra vol

helperFunction :: GramCat a => Eq a => InflectionMap -> (InflectionMap -> Manifest a) -> LexCat -> RVar (Express a)
helperFunction inflMap f lc = out where
  fMan = f inflMap
  lcFind = find (\(ManifestPlace x _ _) -> x == lc) (getManPlaces fMan)
  agreeWith = join $ getAgr <$> lcFind
  out
    | fMan == NoManifest = return NoExpress
    | isJust agreeWith = return $ fromMaybe NoExpress (Agree <$> agreeWith)
    | isJust lcFind = Express <$> choice (getManStuff fMan)
    | otherwise = return NoExpress

-- Fill in agreement
{-
agree :: Phrase -> Phrase


combineGramCatExpress :: GramCatExpress -> GramCatExpress -> GramCatExpress-}
