{-# LANGUAGE NamedFieldPuns #-}
module Translate
( parseParseTree
, parsePhrase
) where

import Prelude hiding (Word)
import Data.List
import Data.Maybe
import Data.Tuple

import GrammarData
import InflectionData
import Parse
import PhonemeData
import Romanization
import EnglishStuff

-- parse parse tree into a string
parseParseTree :: [[Phoneme]] -> [((String, LexCat), Word)] -> [(LexCat, [ManifestSystem], [ManifestSystem], [ManifestSystem])] -> Grammar -> Phrase -> String
parseParseTree sonHier dict systems g pt = "\n\n" ++ roman ++ "\n" ++ native ++ "\n" ++ literal ++ "\n\"" ++ english ++ "\"" where
  leaves   = filter (not.all leafIsNull) (filter (not.null) (parsePhrase g pt))
  eLeaves  = filter (not.all leafIsNull) (filter (not.null) (parsePhrase englishGrammar pt))
  roman    = romanizeLeaves dict leaves
  native   = translateLeaves g sonHier dict systems leaves
  literal  = leavesToEnglish englishGrammar leaves
  english  = leavesToEnglish englishGrammar eLeaves

leafIsInfl :: Leaf -> Bool
leafIsInfl LeafInfl{} = True
leafIsInfl _ = False

leafIsNull :: Leaf -> Bool
leafIsNull LeafNull{} = True
leafIsNull _ = False

-- to english
leavesToEnglish :: Grammar -> [[Leaf]] -> String
leavesToEnglish g leaves = unwords $ map (leavesToEnglish2 g)leaves

leavesToEnglish2 :: Grammar -> [Leaf] -> String
leavesToEnglish2 g leaves
  | any leafIsInfl leaves = inflToEnglish g leaves
  | otherwise             = concatMap leafToEnglish leaves

inflToEnglish :: Grammar -> [Leaf] -> String
inflToEnglish g leaves = out where
  (others, inflLeaves) = break leafIsInfl leaves
  infls = map leafInfl inflLeaves

  (_, mparts, mprefs, msuffs) = fromMaybe (Infl, [], [], []) (find (\(lc, _, _, _) -> lc == leafLC (head others)) englishManifest)

  partCombos = concatMap (\(_,_,x)-> x) mparts
  prefCombos = concatMap (\(_,_,x)-> x) mprefs
  suffCombos = concatMap (\(_,_,x)-> x) msuffs

  parts = map (fst . snd) (filter (\(j,(x,i)) ->  (compareInfl i j)) ((,) <$> infls <*> partCombos))
  prefs = [""]
  suffs = map (fst . snd) (filter (\(j,(x,i)) ->  (compareInfl i j)) ((,) <$> infls <*> suffCombos))

  out
    | getCI g == CompFinal = unwords parts ++ " " ++ concat prefs ++ unwords (map leafToEnglish others) ++ concat suffs
    | otherwise            = concat prefs ++ unwords (map leafToEnglish others) ++ concat suffs ++ " " ++ unwords parts

leafToEnglish :: Leaf -> String
leafToEnglish LeafNull{} = ""
leafToEnglish (Leaf _ _ str) = str
leafToEnglish _ = "ERROR"

-- to new language
translateLeaves :: Grammar -> [[Phoneme]] -> [((String, LexCat), Word)] -> [(LexCat, [ManifestSystem], [ManifestSystem], [ManifestSystem])] -> [[Leaf]] -> String
translateLeaves g sonHier dict systems leaves = unwords $ map (translateLeaves2 g sonHier dict systems) leaves

translateLeaves2 :: Grammar -> [[Phoneme]] -> [((String, LexCat), Word)] -> [(LexCat, [ManifestSystem], [ManifestSystem], [ManifestSystem])] -> [Leaf] -> String
translateLeaves2 g sonHier dict systems leaves
  | any leafIsInfl leaves = translateInfl g sonHier dict systems leaves
  | otherwise             = concatMap (translateLeaf sonHier dict) leaves

translateInfl :: Grammar -> [[Phoneme]] -> [((String, LexCat), Word)] -> [(LexCat, [ManifestSystem], [ManifestSystem], [ManifestSystem])] -> [Leaf] -> String
translateInfl g sonHier dict systems leaves = out where
  (others, inflLeaves) = break leafIsInfl leaves
  infls = map leafInfl inflLeaves

  -- retrieve the relevent particles/prefixes/suffixes from the manifest systems
  (_, mparts, mprefs, msuffs) = fromMaybe (Infl, [], [], []) (find (\(lc, _, _, _) -> lc == leafLC (head inflLeaves)) systems)
  partCombos = concatMap manSysCombos mparts
  prefCombos = concatMap manSysCombos mprefs
  suffCombos = concatMap manSysCombos msuffs
  parts = map (fst . snd) (filter (\(j,(x,i)) ->  (compareInfl i j)) ((,) <$> infls <*> partCombos))
  prefs = map (fst . snd) (filter (\(j,(x,i)) ->  (compareInfl i j)) ((,) <$> infls <*> prefCombos))
  suffs = map (fst . snd) (filter (\(j,(x,i)) ->  (compareInfl i j)) ((,) <$> infls <*> suffCombos))

  -- syllabify the output properly
  partOut = map (parseWord sonHier . Word . (:[])) parts
  othersOut = map (\(Leaf lc _ str) -> fromMaybe (parseWord sonHier (Word suffs) ++ "<UNK>" ++ parseWord sonHier (Word prefs)) (parseWord sonHier . Word <$> ((++ suffs) . (prefs ++) . getMorphemes <$> lookup (str, lc) dict))) others

  out
    | getCI g == CompFinal = unwords partOut ++ " " ++ unwords othersOut
    | otherwise            = unwords othersOut ++ " " ++ unwords partOut

compareInfl :: (Express Gender, Express Animacy, Express Case, Express Number, Express Definiteness, Express Specificity, Express Topic, Express Person, Express Honorific, Express Polarity, Express Tense, Express Aspect, Express Mood, Express Voice, Express Evidentiality, Express Transitivity, Express Volition) -> (Express Gender, Express Animacy, Express Case, Express Number, Express Definiteness, Express Specificity, Express Topic, Express Person, Express Honorific, Express Polarity, Express Tense, Express Aspect, Express Mood, Express Voice, Express Evidentiality, Express Transitivity, Express Volition) -> Bool
compareInfl (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) (gen2,ani2,cas2,num2,def2,spe2,top2,per2,hon2,pol2,ten2,asp2,moo2,voi2,evi2,tra2,vol2)
  | gen /= gen2 && (gen /= NoExpress && gen2 /= NoExpress) = False
  | ani /= ani2 && (ani /= NoExpress && ani2 /= NoExpress) = False
  | cas /= cas2 && (cas /= NoExpress && cas2 /= NoExpress) = False
  | num /= num2 && (num /= NoExpress && num2 /= NoExpress) = False
  | def /= def2 && (def /= NoExpress && def2 /= NoExpress) = False
  | spe /= spe2 && (spe /= NoExpress && spe2 /= NoExpress) = False
  | top /= top2 && (top /= NoExpress && top2 /= NoExpress) = False
  | per /= per2 && (per /= NoExpress && per2 /= NoExpress) = False
  | hon /= hon2 && (hon /= NoExpress && hon2 /= NoExpress) = False
  | pol /= pol2 && (pol /= NoExpress && pol2 /= NoExpress) = False
  | ten /= ten2 && (ten /= NoExpress && ten2 /= NoExpress) = False
  | asp /= asp2 && (asp /= NoExpress && asp2 /= NoExpress) = False
  | moo /= moo2 && (moo /= NoExpress && moo2 /= NoExpress) = False
  | voi /= voi2 && (voi /= NoExpress && voi2 /= NoExpress) = False
  | evi /= evi2 && (evi /= NoExpress && evi2 /= NoExpress) = False
  | tra /= tra2 && (tra /= NoExpress && tra2 /= NoExpress) = False
  | vol /= vol2 && (vol /= NoExpress && vol2 /= NoExpress) = False
  | otherwise = True


translateLeaf :: [[Phoneme]] -> [((String, LexCat), Word)] -> Leaf -> String
translateLeaf _ _ LeafNull{} = ""
translateLeaf sonHier dict (Leaf lc _ str) = translate sonHier dict (str,lc)
translateLeaf _ _ _ = "ERROR"

translate :: [[Phoneme]] -> [((String, LexCat), Word)] -> (String, LexCat) -> String
translate sonHier dict ent = fromMaybe "<UNK>" (parseWord sonHier <$> lookup ent dict)

-- to romanized
romanizeLeaves :: [((String, LexCat), Word)] -> [[Leaf]] -> String
romanizeLeaves dict leaves = unwords $ map (concatMap $ romanizeLeaf dict) leaves

romanizeLeaf :: [((String, LexCat), Word)] -> Leaf -> String
romanizeLeaf _ LeafNull{} = ""
romanizeLeaf dict (Leaf lc _ str) = fromMaybe "<UNK>" (romanizeWord <$> lookup (str, lc) dict)
romanizeLeaf _ (LeafInfl lc infl) = "<UNK>" --we'll get there...


-- parse phrase
parsePhrase :: Grammar -> Phrase -> [[Leaf]]
parsePhrase g (XP Det Null dSpec (XBarC Det Null dHead (XP Noun Null nSpec (XBarA Noun Null nAdjun (XBarC Noun Null nHead nComp))))) = dp where
  dp
    | getSI g == SubInitial = dSpecOut ++ dbar
    | otherwise             = dbar ++ dSpecOut
  dbar
    | getCI g == CompFinal = dHeadOut ++ np
    | otherwise            = np ++ dHeadOut
  np
    | getSI g == SubInitial = nSpecOut ++ nbar1
    | otherwise             = nbar1 ++ nSpecOut
  nbar1 = nAdjunOut ++ nbar2
  nbar2
    | getOF g == ObjFinal = nHeadOut ++ nCompOut
    | otherwise           = nCompOut ++ nHeadOut

  dSpecOut = parsePhrase g dSpec
  dHeadOut = []
  nSpecOut = parsePhrase g nSpec
  nAdjunOut = parsePhrase g nAdjun
  nHeadOut = [[nHead, dHead]]
  nCompOut = parsePhrase g nComp

parsePhrase g (XP Comp _ cSpec (XBarC Comp _ cHead (XP Infl _ iSpec (XBarC Infl _ iHead (XP Verb _ vSpec (XBarC Verb _ vHead vComp)))))) = cp where
  cp
    | getSI g == SubInitial = cSpecOut ++ cbar
    | otherwise             = cbar ++ cSpecOut
  cbar
    | getCI g == CompFinal = cHeadOut ++ ip
    | otherwise            = ip ++ cHeadOut
  ip
    | getSI g == SubInitial = iSpecOut ++ ibar
    | otherwise             = ibar ++ iSpecOut
  ibar
    | getCI g == CompFinal = iHeadOut ++ vp
    | otherwise            = vp ++ iHeadOut
  vp
    | getSI g == SubInitial = vSpecOut ++ vbar
    | otherwise             = vbar ++ vSpecOut
  vbar
    | getOF g == ObjFinal = vHeadOut ++ vCompOut
    | otherwise           = vCompOut ++ vHeadOut

  cSpecOut
    | getWHM g == OblWHMove && phraseIl vComp == Ques && phraseLC vComp == Det = [[Leaf Noun Ques "Who/what"]]
    | getWHM g == OblWHMove && phraseIl vComp == Ques && phraseLC vComp == Adpo = [[Leaf Noun Ques "Where"]]
    | otherwise = parsePhrase g cSpec
  cHeadOut
    | getItoC g == OblItoCMove && cHead == LeafNull Ques = [[iHead]]
    | otherwise = [[cHead]]
  iSpecOut
    | True      = parsePhrase g vSpec
    | otherwise = parsePhrase g iSpec
  iHeadOut
    | getVtoI g == OblVtoIMove && getItoC g == OblItoCMove && cHead == LeafNull Ques = [[vHead]]
    | getItoC g == OblItoCMove && cHead == LeafNull Ques = []
    | getVtoI g == OblVtoIMove = [[vHead, iHead]]
    | getVtoI g == NoVtoIMove && getAH g == OblAffixHop = []
    | otherwise = [[iHead]]
  vSpecOut
    | True      = []
    | otherwise = parsePhrase g vSpec
  vHeadOut
    | getVtoI g == OblVtoIMove = []
    | (getVtoI g == NoVtoIMove && getAH g == OblAffixHop) && (getItoC g /= OblItoCMove || cHead /= LeafNull Ques) = [[vHead, iHead]]
    | otherwise = [[vHead]]
  vCompOut
    | getWHM g == OblWHMove && phraseIl vComp == Ques = []
    | otherwise = parsePhrase g vComp

parsePhrase g (XP lc Ques spec bar)
  | getSI g == SubInitial = parsePhrase g spec ++ parseBar g bar
  | otherwise             = parseBar g bar ++ parsePhrase g spec
parsePhrase g (XP lc _ spec bar)
  | getSI g == SubInitial = parsePhrase g spec ++ parseBar g bar
  | otherwise             = parseBar g bar ++ parsePhrase g spec
parsePhrase g XPNull = []

parseBar :: Grammar -> Bar -> [[Leaf]]
parseBar g (XBarA lc _ adjunct bar) = parsePhrase g adjunct ++ parseBar g bar
parseBar g (XBarC lc _ leaf comp)
  | (lc /= Verb && getCI g == CompFinal) || (lc == Verb && getOF g == ObjFinal) = [leaf] : parsePhrase g comp
  | otherwise            = parsePhrase g comp ++ [[leaf]]
