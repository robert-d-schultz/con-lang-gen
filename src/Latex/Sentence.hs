module Latex.Sentence
( writeLatexParseTrees
, writePhrase
, compareInfl
) where

import ClassyPrelude hiding (Word, (<>))
import Text.LaTeX hiding (unwords)

import Data.Language
import Data.Grammar
import Data.Inflection
import Data.Phoneme
import Data.Word

import Out.Lexicon
import Out.Roman

import EnglishStuff

-- Writes out a parse tree in a few ways:
-- Native writing system (missing)
-- Transliterated (romanized)
-- Transcripted (IPA)
-- Interlinear gloss
-- Literal English translation
-- English translation
writeLatexParseTrees :: Language -> [Phrase] -> LaTeX
writeLatexParseTrees lang pts = subsection (raw "Samples")
                               <> raw "Sample sentences"
                               <> mconcat (zipWith (writeLatexParseTree lang) pts [1..])

-- This makes a table so romanization, ipa, and gloss all line up
writeLatexParseTree :: Language -> Phrase -> Int -> LaTeX
writeLatexParseTree lang pt n = out where
  leaves   = writePhrase lang pt
  eLeaves  = writePhrase englishLanguage pt
  -- native = ...
  literal  = map translateLeaf leaves
  english  = map translateLeaf eLeaves

  romanRow = "Romanized" ++ " & " ++ romanCluster ++ "\\\\ \\hline"
  romanCluster = intercalate " & " roman
  roman = map (romanizeLeaf lang) leaves

  ipaRow = "Transcribed" ++ " & " ++ ipaCluster ++ "\\\\ \\hline"
  ipaCluster = intercalate " & " ipa
  ipa = map (transcribeLeaf lang) leaves

  glossRow = "Glossed" ++ " & " ++ glossCluster ++ "\\\\ \\hline"
  glossCluster = intercalate " & " gloss
  gloss = map (glossLeaf lang) leaves

  caption = "Example " ++ tshow n
  options = caption ++ ", center" ++ ", mincapwidth = 40mm"
  colStuff = "|c*{" ++ tshow (length leaves) ++ "}{c}"
  notes = "Literal: " ++ unwords literal ++ "English: " ++ unwords english
  table = "\\hline" ++ romanRow ++ ipaRow ++ glossRow
  out = raw ("\\ctable[" ++ options ++ "]" ++ "{" ++ colStuff ++ "}" ++ "{" ++ notes ++ "}" ++ "{" ++ table ++ "}")

-- Gloss
glossLeaf :: Language -> Leaf -> Text
glossLeaf  _ LeafNull{} = ""
glossLeaf lang leaf@Leaf{} = glossMorpheme $ leafMorph leaf
glossLeaf lang (LeafInfl _ morphs) = out where
  -- particles only
  partMorphs = filter (\x -> getMorphType x == Particle) morphs
  out = unwords (map glossMorpheme partMorphs)

glossMorpheme :: Morpheme -> Text
glossMorpheme (Word _ morph1 morph2) = glossMorpheme morph1 ++ glossMorpheme morph2
glossMorpheme m@_ = out where
  mt = getMorphType m
  glossed = glossMeaning $ getMeaning m
  out | mt == CTransfix || mt == Transfix = "//" ++ glossed
      | mt == Root = glossed
      | mt == Prefix = glossed ++ "-"
      | mt == Suffix = "-" ++ glossed
      | mt == Particle = " " ++ glossed

glossMeaning :: Meaning -> Text
glossMeaning m@RootMeaning{} = getStr m
glossMeaning m@InflMeaning{} = glossCombo $ getAllExpress m
glossMeaning m@DeriMeaning{} = "DERIVATION" -- gotta be something for this
glossMeaning m@CompMeaning{} = "COMPOUND" -- gotta be something for this
glossMeaning _ = "ERROR?"

glossCombo :: GramCatExpress -> Text
glossCombo (GramCatExpress gen ani cas num def spe top per hon pol ten asp moo voi evi tra vol) = intercalate "." (filter (not.null) l) where
  l = [ glossCombo_ gen
      , glossCombo_ ani
      , glossCombo_ cas
      , glossCombo_ num
      , glossCombo_ def
      , glossCombo_ spe
      , glossCombo_ top
      , glossCombo_ per
      , glossCombo_ hon
      , glossCombo_ pol
      , glossCombo_ ten
      , glossCombo_ asp
      , glossCombo_ moo
      , glossCombo_ voi
      , glossCombo_ evi
      , glossCombo_ tra
      , glossCombo_ vol
      ]

glossCombo_ :: Eq a => GramCat a => Express a -> Text
glossCombo_ NoExpress = ""
glossCombo_ (Agree lc) = "!!!Missing AGR!!!"
glossCombo_ (Express x) = gloss x

-- Transcribe a leaf into IPA
transcribeLeaf :: Language -> Leaf -> Text
transcribeLeaf _ LeafNull{} = ""
transcribeLeaf lang (Leaf _ _ morph) = writeMorphemeIPA lang morph
transcribeLeaf lang (LeafInfl _ morphs) = out where
  -- particles only
  partMorphs = filter (\x -> getMorphType x == Particle) morphs
  out = unwords (map (writeMorphemeIPA lang) partMorphs)

getLast :: [a] -> [a]
getLast [] = []
getLast xs = fromMaybe [] ((:[]) <$> lastMay xs)

-- This checks if the left GramCatExpress helps satisfy the right GramCatExpress
compareInfl :: GramCatExpress -> GramCatExpress -> Bool
compareInfl (GramCatExpress gen ani cas num def spe top per hon pol ten asp moo voi evi tra vol) (GramCatExpress gen2 ani2 cas2 num2 def2 spe2 top2 per2 hon2 pol2 ten2 asp2 moo2 voi2 evi2 tra2 vol2) =
  and [ compareInfl_ gen gen2
      , compareInfl_ ani ani2
      , compareInfl_ cas cas2
      , compareInfl_ num num2
      , compareInfl_ def def2
      , compareInfl_ spe spe2
      , compareInfl_ top top2
      , compareInfl_ per per2
      , compareInfl_ hon hon2
      , compareInfl_ pol pol2
      , compareInfl_ ten ten2
      , compareInfl_ asp asp2
      , compareInfl_ moo moo2
      , compareInfl_ voi voi2
      , compareInfl_ evi evi2
      , compareInfl_ tra tra2
      , compareInfl_ vol vol2
      ]
-- minBound should be UGEN, UANI, UCAS, etc.
compareInfl_ :: Eq a => Bounded a => Express a -> Express a -> Bool
compareInfl_ x y = x == y || x `elem` [Express minBound, NoExpress] -- || (\case Agree{} -> True; _ -> False) x

-- romanize
romanizeLeaf :: Language -> Leaf -> Text
romanizeLeaf _ LeafNull{} = ""
romanizeLeaf lang (Leaf _ _ morph) = romanizeWord lang morph
romanizeLeaf lang (LeafInfl _ morphs) = out where
  -- particles only
  partMorphs = filter (\x -> getMorphType x == Particle) morphs
  out = unwords (map (romanizeWord lang) partMorphs)


translateLeaf :: Leaf -> Text
translateLeaf LeafNull{} = ""
translateLeaf (Leaf _ _ (Word (Meaning lc str gce) _ _)) = unwords out where
  (_, mparts, mprefs, msuffs) = fromMaybe (Infl, [], [], []) (find (\(x, _, _, _) -> (x == lc)) englishManifest)
  partCombos = map (\(_,_,x)-> x) mparts
  prefCombos = map (\(_,_,x)-> x) mprefs
  suffCombos = map (\(_,_,x)-> x) msuffs

  parts = concatMap (getLast . map (fst . snd) . filter (\(j,(_,i)) ->  (compareInfl i j)) . ((,) gce <$>)) partCombos
  prefs = [""] :: [Text] -- i guess english doesn't have any (inflectional) prefixes so this is alright
  suffs = concatMap (getLast . map (fst . snd) . filter (\(j,(_,i)) ->  (compareInfl i j)) . ((,) gce <$>)) suffCombos
  out = parts ++ [str] ++ suffs

translateLeaf _ = "!!ERROR!!"

-- Do insertion

-- Linearizes a parse tree using the grammar specified
-- It outputs a list of [Leaf] where [Leaf] contains a Leaf and any relevent LeafInfl's
-- Definitely not as elegant as it should be
writePhrase :: Language -> Phrase -> [Leaf]
writePhrase lang (XP Det Null dSpec (XBarC Det Null dHead (XP Noun Null nSpec (XBarA Noun Null nAdjun (XBarC Noun Null nHead nComp))))) = dp where
  g = getGrammar lang
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

  dSpecOut = writePhrase lang dSpec
  dHeadOut = [dHead]
  nSpecOut = writePhrase lang nSpec
  nAdjunOut = writePhrase lang nAdjun
  nHeadOut = [composeLeaves nHead dHead]
  nCompOut = writePhrase lang nComp

writePhrase lang (XP Comp _ cSpec (XBarC Comp _ cHead (XP Infl _ iSpec (XBarC Infl _ iHead (XP Verb _ vSpec (XBarC Verb _ vHead vComp)))))) = cp where
  g = getGrammar lang
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
  --  | getWHM g == OblWHMove && phraseIl vComp == Ques && phraseLC vComp == Det  = [Leaf Noun Ques (RootMeaning Inter "Who/what")]
  --  | getWHM g == OblWHMove && phraseIl vComp == Ques && phraseLC vComp == Adpo = [Leaf Noun Ques (RootMeaning Inter "Where")]
    | otherwise                                                                 = writePhrase lang cSpec
  cHeadOut
    | getItoC g == OblItoCMove && cHead == LeafNull Ques = [iHead]
    | otherwise                                          = [cHead]
  iSpecOut
    | True      = writePhrase lang vSpec
    | otherwise = writePhrase lang iSpec
  iHeadOut
    | getVtoI g == OblVtoIMove && getItoC g == OblItoCMove && cHead == LeafNull Ques = [vHead]
    | getItoC g == OblItoCMove && cHead == LeafNull Ques                             = []
    | getVtoI g == OblVtoIMove && getCI g == CompFinal                               = [iHead, composeLeaves vHead iHead]
    | getVtoI g == OblVtoIMove && getCI g == CompInitial                             = [composeLeaves vHead iHead, iHead]
    | getVtoI g == NoVtoIMove && getAH g == OblAffixHop                              = [iHead]
    | getVtoI g == NoVtoIMove && getAH g == NoAffixHop && getCI g == CompFinal       = [iHead, composeLeaves (LeafNull Null) iHead]
    | getVtoI g == NoVtoIMove && getAH g == NoAffixHop && getCI g == CompInitial     = [composeLeaves (LeafNull Null) iHead, iHead]
    | otherwise                                                                      = []
  vSpecOut
    | True      = []
    | otherwise = writePhrase lang vSpec
  vHeadOut
    | getVtoI g == OblVtoIMove                                = []
    | (getVtoI g == NoVtoIMove && getAH g == OblAffixHop)
      && (getItoC g /= OblItoCMove || cHead /= LeafNull Ques) = [composeLeaves vHead iHead]
    | otherwise                                               = [vHead]
  vCompOut
    | getWHM g == OblWHMove && phraseIl vComp == Ques = []
    | otherwise                                       = writePhrase lang vComp

writePhrase lang (XP _ Ques spec bar) = out where
  g = getGrammar lang
  out
    | getSI g == SubInitial = writePhrase lang spec ++ writeBar lang bar
    | otherwise             = writeBar lang bar ++ writePhrase lang spec
writePhrase lang (XP _ _ spec bar) = out where
  g = getGrammar lang
  out
    | getSI g == SubInitial = writePhrase lang spec ++ writeBar lang bar
    | otherwise             = writeBar lang bar ++ writePhrase lang spec
writePhrase _ XPNull = []

writeBar :: Language -> Bar -> [Leaf]
writeBar lang (XBarA _ _ adjunct bar) = writePhrase lang adjunct ++ writeBar lang bar
writeBar lang (XBarC lc _ leaf comp) = out where
  g = getGrammar lang
  out
    | (lc /= Verb && getCI g == CompFinal) || (lc == Verb && getOF g == ObjFinal) = leaf : writePhrase lang comp
    | otherwise            = writePhrase lang comp ++ [leaf]

composeLeaves :: Leaf -> Leaf -> Leaf
compose l LeafNull{} = l
compose LeafNull{} l = l
composeLeaves l1@Leaf{} l2@LeafInfl{} = fromMaybe (LeafNull Null) out where
  morph = foldM applyMorpheme (leafMorph l1) (leafMorphs l2)
  out | leafLC l1 == leafLC l2 = Leaf (leafLC l1) (leafIl l1) <$> morph
      | otherwise = Nothing
composeLeaves l1@LeafInfl{} l2@Leaf{} = fromMaybe (LeafNull Null) out where
  morph = foldM applyMorpheme (leafMorph l2) (leafMorphs l1)
  out | leafLC l2 == leafLC l1 = Leaf (leafLC l2) (leafIl l2) <$> morph
      | otherwise = Nothing
composeLeaves _ _ = LeafNull Decl -- ?
