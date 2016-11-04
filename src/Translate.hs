{-# LANGUAGE NamedFieldPuns #-}
module Translate
( parseParseTree
, parsePhrase
, treeExample
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

treeExample = XP Comp Null XPNull (XBarC Comp Null (LeafNull Null) (XP Infl Null XPNull (XBarC Infl Null (LeafInfl infl) (XP Verb Null themanonthetable (XBarC Verb Null (Leaf Verb Null LeafWord "sit") ontheuglywoman)))))
infl = (NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress)
themanonthetable = XP Det Null XPNull (XBarC Det Null (Leaf Det Null LeafWord "the") (XP Noun Null XPNull (XBarC Noun Null (Leaf Noun Null LeafWord "man") onthetable)))
onthetable = XP Adpo Null XPNull (XBarC Adpo Null (Leaf Adpo Null LeafWord "at") (XP Det Null XPNull (XBarC Det Null (Leaf Det Null LeafWord "the") (XP Noun Null XPNull (XBarC Noun Null (Leaf Noun Null LeafWord "table") XPNull)))))
ontheuglywoman = XP Adpo Null XPNull (XBarC Adpo Null (Leaf Adpo Null LeafWord "on") (XP Det Null XPNull (XBarC Det Null (Leaf Det Null LeafWord "the") (XP Noun Null XPNull (XBarA Noun Null (XP Adj Null XPNull (XBarC Adj Null (Leaf Adj Null LeafWord "ugly") XPNull)) (XBarC Noun Null (Leaf Noun Null LeafWord "woman") XPNull))))))

parseParseTree :: [[Phoneme]] -> [((String, LexCat), Word)] -> Grammar -> Phrase -> String
parseParseTree sonHier dict g pt = roman ++ "\n" ++ native ++ "\n" ++ literal ++ "\n\"" ++ english ++ "\"" where
  leaves   = filter (not.all leafIsNull) (filter (not.null) (parsePhrase g pt))
  eLeaves  = filter (not.all leafIsNull) (filter (not.null) (parsePhrase englishGrammar pt))
  roman    = romanizeLeaves dict leaves
  native   = translateLeaves sonHier dict leaves
  literal  = leavesToEnglish leaves
  english  = leavesToEnglish eLeaves

leavesToEnglish :: [[Leaf]] -> String
leavesToEnglish leaves = unwords $ map (\x -> if leafIsInfl $ head x then "<do>" ++ concatMap leafToEnglish x else concatMap leafToEnglish x ) leaves

leafIsInfl :: Leaf -> Bool
leafIsInfl LeafInfl{} = True
leafIsInfl _ = False

leafIsNull :: Leaf -> Bool
leafIsNull LeafNull{} = True
leafIsNull _ = False

leafToEnglish :: Leaf -> String
leafToEnglish LeafNull{} = ""
leafToEnglish (Leaf _ _ _ str) = str
leafToEnglish (LeafInfl infl) = fromMaybe "<UNK>" (lookup infl (map swap englishVerbInfl))

translateLeaves :: [[Phoneme]] -> [((String, LexCat), Word)] -> [[Leaf]] -> String
translateLeaves sonHier dict leaves = unwords $ map (\x -> if leafIsInfl $ head x then translate sonHier dict ("do", Verb) ++ concatMap (translateLeaf sonHier dict) x else concatMap (translateLeaf sonHier dict) x ) leaves

translateLeaf :: [[Phoneme]] -> [((String, LexCat), Word)] -> Leaf -> String
translateLeaf _ _ LeafNull{} = ""
translateLeaf sonHier dict (Leaf lc _ _ str) = translate sonHier dict (str,lc)
translateLeaf _ _ (LeafInfl infl) = "<UNK>" --we'll get there...

translate :: [[Phoneme]] -> [((String, LexCat), Word)] -> (String, LexCat) -> String
translate sonHier dict ent = fromMaybe "<UNK>" (parseWord sonHier <$> lookup ent dict)

romanizeLeaves :: [((String, LexCat), Word)] -> [[Leaf]] -> String
romanizeLeaves dict leaves = unwords $ map (concatMap $ romanizeLeaf dict) leaves

romanizeLeaf :: [((String, LexCat), Word)] -> Leaf -> String
romanizeLeaf _ LeafNull{} = ""
romanizeLeaf dict (Leaf lc _ _ str) = fromMaybe "<UNK>" (romanizeWord <$> lookup (str, lc) dict)
romanizeLeaf _ (LeafInfl infl) = "<UNK>" --we'll get there...

parsePhrase :: Grammar -> Phrase -> [[Leaf]]
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
    | getWHM g == OblWHMove && phraseIl vComp == Ques && phraseLC vComp == Det = [[Leaf Noun Ques LeafWord "Who/what"]]
    | getWHM g == OblWHMove && phraseIl vComp == Ques && phraseLC vComp == Adpo = [[Leaf Noun Ques LeafWord "Where"]]
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
