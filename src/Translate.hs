{-# LANGUAGE NamedFieldPuns #-}
module Translate
( parseParseTree
, parsePhrase
, treeExample
) where

import Prelude hiding (Word)
import Data.List
import Data.Maybe

import GrammarData
import Parse
import PhonemeData

-- The point here is to translate a parse tree into the target language's grammar system.
-- Worry about lexical translation later
-- Working:
-- SubjectInitial        - "Subject side parameter"
-- ObjectFinal           - Special case of "Head directionality parameter" for V'?
-- ComplementizerInitial - "Head directionality parameter"
-- VtoIMovement          - "Verb raising"
-- AffixHopping          - "Affix lowering"
-- ItoCMovement          - interrogative marked C head

-- Not working:
-- NullSubject           - pronoun related

-- ObligatoryTopic       - topic related
-- NullTopic             - topic related
-- TopicMarking          - topic related

-- WHMovementObligatory  - question related
-- PiedPiping            - question related
-- QuestionInversion     - question related

parseParseTree :: [[Phoneme]] -> [(String, Word)] -> Grammar -> Phrase -> String
parseParseTree sonHier dict g pt = native ++ "\n" ++ literal ++ "\n\"" ++ english ++ "\"" where
  native  = unwords (filter (not.null) (parsePhrase sonHier dict g pt))
  literal = unwords (filter (not.null) (parsePhrase [] dict g pt))
  english = unwords (filter (not.null) (parsePhrase [] dict eg pt))


translate :: [[Phoneme]] -> [(String, Word)] -> String -> String
translate [] dict str = str
translate sonHier dict str = fromMaybe "<UNK>" (parseWord sonHier <$> lookup str dict)


eg = Grammar SubInitial ObjFinal CompFinal OblVtoIMove NoAffixHop NoNullSub OptTopic NoNullTop NoTopMark OblItoCMove OblWHMove PiedPipe OblQuesInv

treeExample = XP Comp Null XPNull (XBarC Comp Null (LeafNull Null) (XP Infl Null XPNull (XBarC Infl Null (Leaf Infl Null LeafAffix "s") (XP Verb Null themanonthetable (XBarC Verb Null (Leaf Verb Null LeafWord "vomit") ontheuglywoman)))))
themanonthetable = XP Det Null XPNull (XBarC Det Null (Leaf Det Null LeafWord "the") (XP Noun Null XPNull (XBarC Noun Null (Leaf Noun Null LeafWord "man") onthetable)))
onthetable = XP Adpo Null XPNull (XBarC Adpo Null (Leaf Adpo Null LeafWord "at") (XP Det Null XPNull (XBarC Det Null (Leaf Det Null LeafWord "the") (XP Noun Null XPNull (XBarC Noun Null (Leaf Noun Null LeafWord "table") XPNull)))))
ontheuglywoman = XP Adpo Null XPNull (XBarC Adpo Null (Leaf Adpo Null LeafWord "on") (XP Det Null XPNull (XBarC Det Null (Leaf Det Null LeafWord "the") (XP Noun Null XPNull (XBarA Noun Null (XP Adj Null XPNull (XBarC Adj Null (Leaf Adj Null LeafWord "ugly") XPNull)) (XBarC Noun Null (Leaf Noun Null LeafWord "woman") XPNull))))))

parsePhrase :: [[Phoneme]] -> [(String, Word)] -> Grammar -> Phrase -> [String]
parsePhrase sonHier dict g (XP Comp _ cSpec (XBarC Comp _ cHead (XP Infl _ iSpec (XBarC Infl _ iHead (XP Verb _ vSpec (XBarC Verb _ vHead vComp)))))) = cp where
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
    | getWHM g == OblWHMove && phraseIl vComp == Ques && phraseLC vComp == Det = ["Who/what"]
    | getWHM g == OblWHMove && phraseIl vComp == Ques && phraseLC vComp == Adpo = ["Where"]
    | otherwise = parsePhrase sonHier dict g cSpec
  cHeadOut
    | getItoC g == OblItoCMove && cHead == LeafNull Ques = parseLeaves sonHier dict g [iHead]
    | otherwise = parseLeaves sonHier dict g [cHead]
  iSpecOut
    | True      = parsePhrase sonHier dict g vSpec
    | otherwise = parsePhrase sonHier dict g iSpec
  iHeadOut
    | getVtoI g == OblVtoIMove && getItoC g == OblItoCMove && cHead == LeafNull Ques = parseLeaves sonHier dict g [vHead]
    | getItoC g == OblItoCMove && cHead == LeafNull Ques = []
    | getVtoI g == OblVtoIMove = parseLeaves sonHier dict g [vHead, iHead]
    | getVtoI g == NoVtoIMove && getAH g == OblAffixHop = []
    | otherwise = parseLeaves sonHier dict g [iHead]
  vSpecOut
    | True      = []
    | otherwise = parsePhrase sonHier dict g vSpec
  vHeadOut
    | getVtoI g == OblVtoIMove = []
    | (getVtoI g == NoVtoIMove && getAH g == OblAffixHop && leafT iHead == LeafAffix) && (getItoC g /= OblItoCMove || cHead /= LeafNull Ques) = parseLeaves sonHier dict g [vHead, iHead]
    | otherwise = parseLeaves sonHier dict g [vHead]
  vCompOut
    | getWHM g == OblWHMove && phraseIl vComp == Ques = []
    | otherwise = parsePhrase sonHier dict g vComp

parsePhrase sonHier dict g (XP lc Ques spec bar)
  | getSI g == SubInitial = parsePhrase sonHier dict g spec ++ parseBar sonHier dict g bar
  | otherwise             = parseBar sonHier dict g bar ++ parsePhrase sonHier dict g spec
parsePhrase sonHier dict g (XP lc _ spec bar)
  | getSI g == SubInitial = parsePhrase sonHier dict g spec ++ parseBar sonHier dict g bar
  | otherwise             = parseBar sonHier dict g bar ++ parsePhrase sonHier dict g spec
parsePhrase sonHier dict g XPNull = []

parseBar :: [[Phoneme]] -> [(String, Word)] -> Grammar -> Bar -> [String]
parseBar sonHier dict g (XBarA lc _ adjunct bar) = parsePhrase sonHier dict g adjunct ++ parseBar sonHier dict g bar
parseBar sonHier dict g (XBarC lc _ leaf comp)
  | (lc /= Verb && getCI g == CompFinal) || (lc == Verb && getOF g == ObjFinal) = parseLeaves sonHier dict g [leaf] ++ parsePhrase sonHier dict g comp
  | otherwise            = parsePhrase sonHier dict g comp ++ parseLeaves sonHier dict g [leaf]

parseLeaves :: [[Phoneme]] -> [(String, Word)] -> Grammar -> [Leaf] -> [String]
parseLeaves _ _ _ [] = [""]
parseLeaves sonHier dict g leaves
  | leafisNull $ last leaves         = parseLeaves sonHier dict g (init leaves)
  | length leaves == 1 && leafT (last leaves) == LeafAffix = ["<do>" ++ translate sonHier dict (leafStr $ last leaves)]
  | leafT (last leaves) == LeafAffix = init (parseLeaves sonHier dict g (init leaves)) ++ [last (parseLeaves sonHier dict g (init leaves)) ++  translate sonHier dict (leafStr $ last leaves)]
  | otherwise                        = parseLeaves sonHier dict g (init leaves) ++ [translate sonHier dict (leafStr $ last leaves)]

leafisNull :: Leaf -> Bool
leafisNull LeafNull{} = True
leafisNull _ = False
