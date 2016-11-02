{-# LANGUAGE NamedFieldPuns #-}
module Translate
( testExample
, parsePhrase
) where

import Data.List

import GrammarData

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

testExample :: Grammar -> String
testExample grammar = unwords (filter (not.null) (parsePhrase grammar underlyingExample))

engGrammar = Grammar SubInitial ObjFinal CompFinal OblVtoIMove NoAffixHop NoNullSub OptTopic NoNullTop NoTopMark OblItoCMove OblWHMove PiedPipe OblQuesInv

underlyingExample = XP Comp Null XPNull (XBarC Comp Null (LeafNull Null) (XP Infl Null XPNull (XBarC Infl Null (Leaf Infl Null LeafAffix "s") (XP Verb Null themanonthetable (XBarC Verb Null (Leaf Verb Null LeafWord "vomit") ontheuglywoman)))))
themanonthetable = XP Det Null XPNull (XBarC Det Null (Leaf Det Null LeafWord "the") (XP Noun Null XPNull (XBarC Noun Null (Leaf Noun Null LeafWord "man") onthetable)))
onthetable = XP Adpo Null XPNull (XBarC Adpo Null (Leaf Adpo Null LeafWord "at") (XP Det Null XPNull (XBarC Det Null (Leaf Det Null LeafWord "the") (XP Noun Null XPNull (XBarC Noun Null (Leaf Noun Null LeafWord "table") XPNull)))))
ontheuglywoman = XP Adpo Null XPNull (XBarC Adpo Null (Leaf Adpo Null LeafWord "on") (XP Det Null XPNull (XBarC Det Null (Leaf Det Null LeafWord "the") (XP Noun Null XPNull (XBarA Noun Null (XP Adj Null XPNull (XBarC Adj Null (Leaf Adj Null LeafWord "ugly") XPNull)) (XBarC Noun Null (Leaf Noun Null LeafWord "woman") XPNull))))))

parsePhrase :: Grammar -> Phrase -> [String]
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
    | getWHM g == OblWHMove && phraseIl vComp == Ques && phraseLC vComp == Det = ["Who/what"]
    | getWHM g == OblWHMove && phraseIl vComp == Ques && phraseLC vComp == Adpo = ["Where"]
    | otherwise = parsePhrase g cSpec
  cHeadOut
    | getItoC g == OblItoCMove && cHead == LeafNull Ques = parseLeaves g [iHead]
    | otherwise = parseLeaves g [cHead]
  iSpecOut
    | True      = parsePhrase g vSpec
    | otherwise = parsePhrase g iSpec
  iHeadOut
    | getVtoI g == OblVtoIMove && getItoC g == OblItoCMove && cHead == LeafNull Ques = parseLeaves g [vHead]
    | getItoC g == OblItoCMove && cHead == LeafNull Ques = []
    | getVtoI g == OblVtoIMove = parseLeaves g [vHead, iHead]
    | getVtoI g == NoVtoIMove && getAH g == OblAffixHop = []
    | otherwise = parseLeaves g [iHead]
  vSpecOut
    | True      = []
    | otherwise = parsePhrase g vSpec
  vHeadOut
    | getVtoI g == OblVtoIMove = []
    | (getVtoI g == NoVtoIMove && getAH g == OblAffixHop && leafT iHead == LeafAffix) && (getItoC g /= OblItoCMove || cHead /= LeafNull Ques) = parseLeaves g [vHead, iHead]
    | otherwise = parseLeaves g [vHead]
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

parseBar :: Grammar -> Bar -> [String]
parseBar g (XBarA lc _ adjunct bar) = parsePhrase g adjunct ++ parseBar g bar
parseBar g (XBarC lc _ leaf comp)
  | (lc /= Verb && getCI g == CompFinal) || (lc == Verb && getOF g == ObjFinal) = parseLeaves g [leaf] ++ parsePhrase g comp
  | otherwise            = parsePhrase g comp ++ parseLeaves g [leaf]

parseLeaves :: Grammar -> [Leaf] -> [String]
parseLeaves _ [] = [""]
parseLeaves g leaves
  | leafisNull $ last leaves         = parseLeaves g (init leaves)
  | length leaves == 1 && leafT (last leaves) == LeafAffix = ["<do>" ++ leafStr (last leaves)]
  | leafT (last leaves) == LeafAffix = init (parseLeaves g (init leaves)) ++ [last (parseLeaves g (init leaves)) ++ leafStr (last leaves)]
  | otherwise                        = parseLeaves g (init leaves) ++ [leafStr $ last leaves]

leafisNull :: Leaf -> Bool
leafisNull LeafNull{} = True
leafisNull _ = False
