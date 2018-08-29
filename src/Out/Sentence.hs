{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Out.Sentence
( writeParseTree
, writePhrase
) where

import ClassyPrelude hiding (Word)

import Data.Language
import Data.Grammar
import Data.Inflection
import Data.Phoneme
import Out.Roman
import EnglishStuff

import Out.Lexicon

-- writes out a parse tree in a few ways:
-- native writing system (missing)
-- transliterated (romanized)
-- transcripted (IPA)
-- interlinear gloss
-- literal english translation
-- english translation
writeParseTree :: Language -> [((Text, LexCat), SyllWord)] -> [ManifestSystem] -> Phrase -> Text
writeParseTree lang dict infl pt = "\n<br>\n" ++ table ++ "<br>\n" ++ literal ++ "\n<br>\n\"" ++ english ++ "\"" where
  leaves   = filter (not.all leafIsNull) (filter (not.null) (writePhrase lang pt))
  eLeaves  = filter (not.all leafIsNull) (filter (not.null) (writePhrase englishLanguage pt))
  -- native = ...
  table    = tableLeaves lang dict infl leaves
  literal  = translateLeaves leaves
  english  = translateLeaves eLeaves

-- makes a table so romanization, ipa, and gloss all line up
tableLeaves :: Language -> [((Text, LexCat), SyllWord)] -> [ManifestSystem] -> [[Leaf]] -> Text
tableLeaves lang dict infl leaves = "\n<table border=1>" ++ tHeader ++ romanRow ++ ipaRow ++ glossRow ++ "\n</table>\n" where
  tHeader = "\n\t<tr>\n\t\t<th colspan=\"" ++ tshow (length leaves + 1) ++ "\">Example</th>\n\t</tr>"

  romanRow = "\n\t<tr>\n\t\t<th>" ++ "Romanized" ++ "</th>" ++ romanCluster ++ "\n\t</tr>"
  romanCluster = "\n\t\t<td>" ++ intercalate "</td>\n\t\t<td>" roman ++ "</td>"
  roman = map (romanizeLeaves dict infl) leaves

  ipaRow = "\n\t<tr>\n\t\t<th>" ++ "Transcribed" ++ "</th>" ++ ipaCluster ++ "\n\t</tr>"
  ipaCluster = "\n\t\t<td>" ++ intercalate "</td>\n\t\t<td>" ipa ++ "</td>"
  ipa = map (transcribeLeaves lang dict infl) leaves

  glossRow = "\n\t<tr>\n\t\t<th>" ++ "Glossed" ++ "</th>" ++ glossCluster ++ "\n\t</tr>"
  glossCluster = "\n\t\t<td>" ++ intercalate "</td>\n\t\t<td>" gloss ++ "</td>"
  gloss = map (glossLeaves infl) leaves

leafIsInfl :: Leaf -> Bool
leafIsInfl LeafInfl{} = True
leafIsInfl _ = False

leafIsNull :: Leaf -> Bool
leafIsNull LeafNull{} = True
leafIsNull _ = False

-- gloss
glossLeaves :: [ManifestSystem] -> [Leaf] -> Text
glossLeaves infl leaves
  | any leafIsInfl leaves = glossInfl infl leaves
  | otherwise             = concatMap translateLeaf leaves

glossInfl :: [ManifestSystem] -> [Leaf] -> Text
glossInfl infl leaves = out where
  (others, inflLeaves)  = break leafIsInfl leaves
  infls                 = map leafInfl inflLeaves

  -- retrieve the relevent particles/prefixes/suffixes from the manifest systems
  (partCombos, prefCombos, suffCombos) = newFunction2 infl inflLeaves
  parts = concatMap (getLast . map (snd . snd) . filter (\(j,(_,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) partCombos
  prefs = concatMap (getLast . map (snd . snd) . filter (\(j,(_,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) prefCombos
  suffs = concatMap (getLast . map (snd . snd) . filter (\(j,(_,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) suffCombos
  partsOut = map glossCombo parts
  prefsOut = map glossCombo prefs
  suffsOut = map glossCombo suffs

  out
    | null others = unwords partsOut
    | otherwise   = intercalate "-" prefsOut ++ (if null prefs then "" else "-") ++ unwords (map translateLeaf2 others) ++ (if null suffs then "" else "-") ++ intercalate "-" suffsOut


newFunction :: [ManifestSystem] -> [Leaf] -> ([SyllWord], [SyllWord], [SyllWord], [Leaf])
newFunction infl leaves = (parts, prefs, suffs, others) where
  (others, inflLeaves)  = break leafIsInfl leaves
  infls                 = map leafInfl inflLeaves

  -- retrieve the relevent particles/prefixes/suffixes from the manifest systems
  (partCombos, prefCombos, suffCombos) = newFunction2 infl inflLeaves
  parts = concatMap (getLast . map (fst . snd) . filter (\(j,(_,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) partCombos
  prefs = concatMap (getLast . map (fst . snd) . filter (\(j,(_,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) prefCombos
  suffs = concatMap (getLast . map (fst . snd) . filter (\(j,(_,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) suffCombos

newFunction2 :: [ManifestSystem] -> [Leaf] -> ([[(SyllWord, AllExpress)]], [[(SyllWord, AllExpress)]], [[(SyllWord, AllExpress)]])
newFunction2 infl inflLeaves = (partCombos, prefCombos, suffCombos) where
  minfl = filter (\x -> fromMaybe False ((==) (manSysLC x) . leafLC <$> listToMaybe inflLeaves)) infl
  mparts = filter (\x -> manSysType x == Particle) minfl
  mprefs = filter (\x -> manSysType x == Prefix) minfl
  msuffs = filter (\x -> manSysType x == Suffix) minfl
  partCombos = map manSysCombos mparts
  prefCombos = map manSysCombos mprefs
  suffCombos = map manSysCombos msuffs

glossCombo :: AllExpress -> Text
glossCombo (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) = intercalate "." vol2 where
  gen2 | gen /= NoExpress = [tshow $ getExp gen]      | otherwise = []
  ani2 | ani /= NoExpress = tshow (getExp ani) : gen2 | otherwise = gen2
  cas2 | cas /= NoExpress = tshow (getExp cas) : ani2 | otherwise = ani2
  num2 | num /= NoExpress = tshow (getExp num) : cas2 | otherwise = cas2
  def2 | def /= NoExpress = tshow (getExp def) : num2 | otherwise = num2
  spe2 | spe /= NoExpress = tshow (getExp spe) : def2 | otherwise = def2
  top2 | top /= NoExpress = tshow (getExp top) : spe2 | otherwise = spe2
  per2 | per /= NoExpress = tshow (getExp per) : top2 | otherwise = top2
  hon2 | hon /= NoExpress = tshow (getExp hon) : per2 | otherwise = per2
  pol2 | pol /= NoExpress = tshow (getExp pol) : hon2 | otherwise = hon2
  ten2 | ten /= NoExpress = tshow (getExp ten) : pol2 | otherwise = pol2
  asp2 | asp /= NoExpress = tshow (getExp asp) : ten2 | otherwise = ten2
  moo2 | moo /= NoExpress = tshow (getExp moo) : asp2 | otherwise = asp2
  voi2 | voi /= NoExpress = tshow (getExp voi) : moo2 | otherwise = moo2
  evi2 | evi /= NoExpress = tshow (getExp evi) : voi2 | otherwise = voi2
  tra2 | tra /= NoExpress = tshow (getExp tra) : evi2 | otherwise = evi2
  vol2 | vol /= NoExpress = tshow (getExp vol) : tra2 | otherwise = tra2

-- transcribe sentence into IPA
transcribeLeaves :: Language -> [((Text, LexCat), SyllWord)] -> [ManifestSystem] -> [Leaf] -> Text
transcribeLeaves lang dict infl leaves
  | any leafIsInfl leaves = transcribeInfl lang dict infl leaves
  | otherwise             = concatMap (transcribeLeaf lang dict) leaves

transcribeInfl :: Language -> [((Text, LexCat), SyllWord)] -> [ManifestSystem] -> [Leaf] -> Text
transcribeInfl lang dict infl leaves = out where
  (parts, prefs, suffs, others) = newFunction infl leaves

  -- syllabify the output properly
  partOut = map writeSyllWordIPA parts
  othersOut = map (\x -> case x of (Leaf lc _ str) -> fromMaybe (concatMap writeSyllWordIPA suffs ++ "<UNK>" ++ concatMap writeSyllWordIPA prefs) (concatMap writeSyllWordIPA <$> ((++ suffs) . (prefs ++) . (:[]) <$> lookup (str, lc) dict))
                                   (LeafNull _) -> ""
                                   _ -> ""
                                   ) others

  out
    | null others          = unwords partOut
    | otherwise            = unwords othersOut

getLast :: [a] -> [a]
getLast [] = []
getLast xs = fromMaybe [] ((:[]) <$> lastMay xs)

-- compares new language infl (1) to english infl (2)
compareInfl :: (Express Gender, Express Animacy, Express Case, Express Number, Express Definiteness, Express Specificity, Express Topic, Express Person, Express Honorific, Express Polarity, Express Tense, Express Aspect, Express Mood, Express Voice, Express Evidentiality, Express Transitivity, Express Volition) -> (Express Gender, Express Animacy, Express Case, Express Number, Express Definiteness, Express Specificity, Express Topic, Express Person, Express Honorific, Express Polarity, Express Tense, Express Aspect, Express Mood, Express Voice, Express Evidentiality, Express Transitivity, Express Volition) -> Bool
compareInfl (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) (gen2,ani2,cas2,num2,def2,spe2,top2,per2,hon2,pol2,ten2,asp2,moo2,voi2,evi2,tra2,vol2) =
  and [ gen == gen2 || gen `elem` [Express UGEN, NoExpress]
      , ani == ani2 || ani `elem` [Express UANI, NoExpress]
      , cas == cas2 || cas `elem` [Express UCAS, NoExpress]
      , num == num2 || num `elem` [Express UNUM, NoExpress]
      , def == def2 || def `elem` [Express UDEF, NoExpress]
      , spe == spe2 || spe `elem` [Express USPE, NoExpress]
      , top == top2 || top `elem` [Express UTOP, NoExpress]
      , per == per2 || per `elem` [Express UPER, NoExpress]
      , hon == hon2 || hon `elem` [Express UHON, NoExpress]
      , pol == pol2 || pol `elem` [Express UPOL, NoExpress]
      , ten == ten2 || ten `elem` [Express UTEN, NoExpress]
      , asp == asp2 || asp `elem` [Express UASP, NoExpress]
      , moo == moo2 || moo `elem` [Express UMOO, NoExpress]
      , voi == voi2 || voi `elem` [Express UVOI, NoExpress]
      , evi == evi2 || evi `elem` [Express UEVI, NoExpress]
      , tra == tra2 || tra `elem` [Express UTRA, NoExpress]
      , vol == vol2 || vol `elem` [Express UVOL, NoExpress]
      ]

transcribeLeaf :: Language -> [((Text, LexCat), SyllWord)] -> Leaf -> Text
transcribeLeaf _ _ LeafNull{} = ""
transcribeLeaf lang dict (Leaf lc _ str) = transcribe lang dict (str,lc)
transcribeLeaf _ _ _ = "ERROR"

transcribe :: Language -> [((Text, LexCat), SyllWord)] -> (Text, LexCat) -> Text
transcribe lang dict ent = fromMaybe "<UNK>" (writeSyllWordIPA <$> lookup ent dict)


-- romanize
romanizeLeaves :: [((Text, LexCat), SyllWord)] -> [ManifestSystem] -> [Leaf] -> Text
romanizeLeaves dict infl leaves
  | any leafIsInfl leaves = romanizeInfl dict infl leaves
  | otherwise             = concatMap (romanizeLeaf dict) leaves

romanizeInfl :: [((Text, LexCat), SyllWord)] -> [ManifestSystem] -> [Leaf] -> Text
romanizeInfl dict infl leaves = out where
  (parts, prefs, suffs, others) = newFunction infl leaves

  -- syllabify the output properly
  partOut = map romanizeSyllWord parts
  othersOut = map (\x -> case x of (Leaf lc _ str) -> fromMaybe (concatMap romanizeSyllWord suffs ++ "<UNK>" ++  concatMap romanizeSyllWord prefs) (concatMap romanizeSyllWord <$> ((++ suffs) . (prefs ++) . (:[]) <$> lookup (str, lc) dict))
                                   (LeafNull _) -> ""
                                   _ -> ""
                                   ) others

  out
    | null others          = unwords partOut
    | otherwise            = unwords othersOut

romanizeLeaf :: [((Text, LexCat), SyllWord)] -> Leaf -> Text
romanizeLeaf _ LeafNull{} = ""
romanizeLeaf dict (Leaf lc _ str) = fromMaybe "<UNK>" (romanizeSyllWord <$> lookup (str, lc) dict)
romanizeLeaf _ _ = "ERROR"


-- (tries to) translate to English
translateLeaves :: [[Leaf]] -> Text
translateLeaves leaves = unwords $ map translateLeaves2 leaves

translateLeaves2 :: [Leaf] -> Text
translateLeaves2 leaves
  | any leafIsInfl leaves = translateInfl leaves
  | otherwise             = concatMap translateLeaf leaves

translateInfl :: [Leaf] -> Text
translateInfl leaves = out where
  (others, inflLeaves) = break leafIsInfl leaves
  infls = map leafInfl inflLeaves

  (_, mparts, mprefs, msuffs) = fromMaybe (Infl, [], [], []) (find (\(lc, _, _, _) -> fromMaybe False ((==) lc . leafLC <$> listToMaybe inflLeaves)) englishManifest)

  partCombos = map (\(_,_,x)-> x) mparts
  prefCombos = map (\(_,_,x)-> x) mprefs
  suffCombos = map (\(_,_,x)-> x) msuffs

  parts = (concatMap (getLast . map (fst . snd) . filter (\(j,(_,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) partCombos) :: [Text]
  prefs = [""] :: [Text] -- i guess english doesn't have any (inflectional) prefixes so this is alright
  suffs = (concatMap (getLast . map (fst . snd) . filter (\(j,(_,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) suffCombos) :: [Text]

  out
    | null others = unwords parts
    | otherwise   = concat prefs ++ unwords (map translateLeaf2 others) ++ concat suffs

translateLeaf :: Leaf -> Text
translateLeaf LeafNull{} = ""
translateLeaf (Leaf _ _ str) = str
translateLeaf _ = "ERROR"

-- do insertion
translateLeaf2 :: Leaf -> Text
translateLeaf2 LeafNull{} = "do"
translateLeaf2 (Leaf _ _ str) = str
translateLeaf2 _ = "ERROR"


-- linearizes a parse tree using the grammar specified
-- it outputs a list of [Leaf] where [Leaf] contains a Leaf and any relevent LeafInfl's
-- definitely not as elegant as it should be
writePhrase :: Language -> Phrase -> [[Leaf]]
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
  dHeadOut = [[dHead]]
  nSpecOut = writePhrase lang nSpec
  nAdjunOut = writePhrase lang nAdjun
  nHeadOut = [[nHead, dHead]]
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
    | getWHM g == OblWHMove && phraseIl vComp == Ques && phraseLC vComp == Det  = [[Leaf Noun Ques "Who/what"]]
    | getWHM g == OblWHMove && phraseIl vComp == Ques && phraseLC vComp == Adpo = [[Leaf Noun Ques "Where"]]
    | otherwise                                                                 = writePhrase lang cSpec
  cHeadOut
    | getItoC g == OblItoCMove && cHead == LeafNull Ques = [[iHead]]
    | otherwise                                          = [[cHead]]
  iSpecOut
    | True      = writePhrase lang vSpec
    | otherwise = writePhrase lang iSpec
  iHeadOut
    | getVtoI g == OblVtoIMove && getItoC g == OblItoCMove && cHead == LeafNull Ques = [[vHead]]
    | getItoC g == OblItoCMove && cHead == LeafNull Ques                             = []
    | getVtoI g == OblVtoIMove && getCI g == CompFinal                               = [[iHead], [vHead, iHead]]
    | getVtoI g == OblVtoIMove && getCI g == CompInitial                             = [[vHead, iHead], [iHead]]
    | getVtoI g == NoVtoIMove && getAH g == OblAffixHop                              = [[iHead]]
    | getVtoI g == NoVtoIMove && getAH g == NoAffixHop && getCI g == CompFinal       = [[iHead], [LeafNull Null, iHead]]
    | getVtoI g == NoVtoIMove && getAH g == NoAffixHop && getCI g == CompInitial     = [[LeafNull Null, iHead], [iHead]]
    | otherwise                                                                      = []
  vSpecOut
    | True      = []
    | otherwise = writePhrase lang vSpec
  vHeadOut
    | getVtoI g == OblVtoIMove                                = []
    | (getVtoI g == NoVtoIMove && getAH g == OblAffixHop)
      && (getItoC g /= OblItoCMove || cHead /= LeafNull Ques) = [[vHead, iHead]]
    | otherwise                                               = [[vHead]]
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

writeBar :: Language -> Bar -> [[Leaf]]
writeBar lang (XBarA _ _ adjunct bar) = writePhrase lang adjunct ++ writeBar lang bar
writeBar lang (XBarC lc _ leaf comp) = out where
  g = getGrammar lang
  out
    | (lc /= Verb && getCI g == CompFinal) || (lc == Verb && getOF g == ObjFinal) = [leaf] : writePhrase lang comp
    | otherwise            = writePhrase lang comp ++ [[leaf]]
