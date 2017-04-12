{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Out.Sentence
( parseParseTree
, parsePhrase
) where

import ClassyPrelude hiding (Word)

import Data.Grammar
import Data.Inflection
import Data.Phoneme
import Out.Roman
import EnglishStuff

import Out.Lexicon

-- parse parse tree into a string
parseParseTree :: [[Phoneme]] -> [((Text, LexCat), Morpheme)] -> [ManifestSystem] -> Grammar -> Phrase -> Text
parseParseTree sonHier dict infl g pt = "\n\n" ++ roman ++ "\n" ++ native ++ "\n" ++ gloss ++ "\n" ++ literal ++ "\n\"" ++ english ++ "\"" where
  leaves   = filter (not.all leafIsNull) (filter (not.null) (parsePhrase g pt))
  eLeaves  = filter (not.all leafIsNull) (filter (not.null) (parsePhrase englishGrammar pt))
  roman    = romanizeLeaves g dict infl leaves
  native   = translateLeaves g sonHier dict infl leaves
  gloss    = glossLeaves infl leaves
  literal  = leavesToEnglish leaves
  english  = leavesToEnglish eLeaves

leafIsInfl :: Leaf -> Bool
leafIsInfl LeafInfl{} = True
leafIsInfl _ = False

leafIsNull :: Leaf -> Bool
leafIsNull LeafNull{} = True
leafIsNull _ = False

-- gloss
glossLeaves :: [ManifestSystem] -> [[Leaf]] -> Text
glossLeaves infl leaves = unwords $ map (glossLeaves2 infl) leaves

glossLeaves2 :: [ManifestSystem] -> [Leaf] -> Text
glossLeaves2 infl leaves
  | any leafIsInfl leaves = glossInfl infl leaves
  | otherwise             = concatMap leafToEnglish leaves

glossInfl :: [ManifestSystem] -> [Leaf] -> Text
glossInfl infl leaves = out where
  (others, inflLeaves)   = break leafIsInfl leaves
  infls                  = map leafInfl inflLeaves

  -- retrieve the relevent particles/prefixes/suffixes from the manifest systems
  minfl = filter (\x -> fromMaybe False ((==) (manSysLC x) . leafLC <$> listToMaybe inflLeaves)) infl
  mparts = filter (\x -> manSysType x == Particle) minfl
  mprefs = filter (\x -> manSysType x == Prefix) minfl
  msuffs = filter (\x -> manSysType x == Prefix) minfl
  partCombos = map manSysCombos mparts
  prefCombos = map manSysCombos mprefs
  suffCombos = map manSysCombos msuffs
  parts = concatMap (getLast . map (snd . snd) . filter (\(j,(x,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) partCombos
  prefs = concatMap (getLast . map (snd . snd) . filter (\(j,(x,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) prefCombos
  suffs = concatMap (getLast . map (snd . snd) . filter (\(j,(x,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) suffCombos

  partsOut = map glossCombo parts
  prefsOut = map glossCombo prefs
  suffsOut = map glossCombo suffs

  out
    | null others = unwords partsOut
    | otherwise   = intercalate "-" prefsOut ++ (if null prefs then "" else "-") ++ unwords (map leafToEnglish2 others) ++ (if null suffs then "" else "-") ++ intercalate "-" suffsOut

glossCombo :: (Express Gender, Express Animacy, Express Case, Express Number, Express Definiteness, Express Specificity, Express Topic, Express Person, Express Honorific, Express Polarity, Express Tense, Express Aspect, Express Mood, Express Voice, Express Evidentiality, Express Transitivity, Express Volition) -> Text
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

-- to english
leavesToEnglish :: [[Leaf]] -> Text
leavesToEnglish leaves = unwords $ map leavesToEnglish2 leaves

leavesToEnglish2 :: [Leaf] -> Text
leavesToEnglish2 leaves
  | any leafIsInfl leaves = inflToEnglish leaves
  | otherwise             = concatMap leafToEnglish leaves

inflToEnglish :: [Leaf] -> Text
inflToEnglish leaves = out where
  (others, inflLeaves) = break leafIsInfl leaves
  infls = map leafInfl inflLeaves

  (_, mparts, mprefs, msuffs) = fromMaybe (Infl, [], [], []) (find (\(lc, _, _, _) -> fromMaybe False ((==) lc . leafLC <$> listToMaybe inflLeaves)) englishManifest)

  partCombos = map (\(_,_,x)-> x) mparts
  prefCombos = map (\(_,_,x)-> x) mprefs
  suffCombos = map (\(_,_,x)-> x) msuffs

  parts = (concatMap (getLast . map (fst . snd) . filter (\(j,(x,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) partCombos) :: [Text]
  prefs = [""] :: [Text]
  suffs = (concatMap (getLast . map (fst . snd) . filter (\(j,(x,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) suffCombos) :: [Text]

  out
    | null others = unwords parts
    | otherwise   = concat prefs ++ unwords (map leafToEnglish2 others) ++ concat suffs

leafToEnglish :: Leaf -> Text
leafToEnglish LeafNull{} = ""
leafToEnglish (Leaf _ _ str) = str
leafToEnglish _ = "ERROR"

-- do insertion
leafToEnglish2 :: Leaf -> Text
leafToEnglish2 LeafNull{} = "do"
leafToEnglish2 (Leaf _ _ str) = str
leafToEnglish2 _ = "ERROR"

-- to new language
translateLeaves :: Grammar -> [[Phoneme]] -> [((Text, LexCat), Morpheme)] -> [ManifestSystem] -> [[Leaf]] -> Text
translateLeaves g sonHier dict infl leaves = unwords $ map (translateLeaves2 g sonHier dict infl) leaves

translateLeaves2 :: Grammar -> [[Phoneme]] -> [((Text, LexCat), Morpheme)] -> [ManifestSystem] -> [Leaf] -> Text
translateLeaves2 g sonHier dict infl leaves
  | any leafIsInfl leaves = translateInfl g sonHier dict infl leaves
  | otherwise             = concatMap (translateLeaf sonHier dict) leaves

translateInfl :: Grammar -> [[Phoneme]] -> [((Text, LexCat), Morpheme)] -> [ManifestSystem] -> [Leaf] -> Text
translateInfl g sonHier dict infl leaves = out where
  (others, inflLeaves) = break leafIsInfl leaves
  infls = map leafInfl inflLeaves

  -- retrieve the relevent particles/prefixes/suffixes from the manifest systems
  minfl = filter (\x -> fromMaybe False ((==) (manSysLC x) . leafLC <$> listToMaybe inflLeaves)) infl
  mparts = filter (\x -> manSysType x == Particle) minfl
  mprefs = filter (\x -> manSysType x == Prefix) minfl
  msuffs = filter (\x -> manSysType x == Prefix) minfl
  partCombos = map manSysCombos mparts
  prefCombos = map manSysCombos mprefs
  suffCombos = map manSysCombos msuffs
  parts = concatMap (getLast . map (fst . snd) . filter (\(j,(x,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) partCombos
  prefs = concatMap (getLast . map (fst . snd) . filter (\(j,(x,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) prefCombos
  suffs = concatMap (getLast . map (fst . snd) . filter (\(j,(x,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) suffCombos

  -- syllabify the output properly
  partOut = map (parseWordIPA sonHier . Word . (:[])) parts
  othersOut = map (\x -> case x of (Leaf lc _ str) -> fromMaybe (parseWordIPA sonHier (Word suffs) ++ "<UNK>" ++ parseWordIPA sonHier (Word prefs)) (parseWordIPA sonHier . Word <$> ((++ suffs) . (prefs ++) . (:[]) <$> lookup (str, lc) dict))
                                   (LeafNull _) -> "") others

  out
    | null others          = unwords partOut
    | otherwise            = unwords othersOut

getLast :: [a] -> [a]
getLast [] = []
getLast xs = fromMaybe [] ((:[]) <$> lastMay xs)

-- compares new language infl (1) to english infl (2)
compareInfl :: (Express Gender, Express Animacy, Express Case, Express Number, Express Definiteness, Express Specificity, Express Topic, Express Person, Express Honorific, Express Polarity, Express Tense, Express Aspect, Express Mood, Express Voice, Express Evidentiality, Express Transitivity, Express Volition) -> (Express Gender, Express Animacy, Express Case, Express Number, Express Definiteness, Express Specificity, Express Topic, Express Person, Express Honorific, Express Polarity, Express Tense, Express Aspect, Express Mood, Express Voice, Express Evidentiality, Express Transitivity, Express Volition) -> Bool
compareInfl (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) (gen2,ani2,cas2,num2,def2,spe2,top2,per2,hon2,pol2,ten2,asp2,moo2,voi2,evi2,tra2,vol2) =
  and [ (gen == gen2 || gen `elem` [Express UGEN, NoExpress])
      , (ani == ani2 || ani `elem` [Express UANI, NoExpress])
      , (cas == cas2 || cas `elem` [Express UCAS, NoExpress])
      , (num == num2 || num `elem` [Express UNUM, NoExpress])
      , (def == def2 || def `elem` [Express UDEF, NoExpress])
      , (spe == spe2 || spe `elem` [Express USPE, NoExpress])
      , (top == top2 || top `elem` [Express UTOP, NoExpress])
      , (per == per2 || per `elem` [Express UPER, NoExpress])
      , (hon == hon2 || hon `elem` [Express UHON, NoExpress])
      , (pol == pol2 || pol `elem` [Express UPOL, NoExpress])
      , (ten == ten2 || ten `elem` [Express UTEN, NoExpress])
      , (asp == asp2 || asp `elem` [Express UASP, NoExpress])
      , (moo == moo2 || moo `elem` [Express UMOO, NoExpress])
      , (voi == voi2 || voi `elem` [Express UVOI, NoExpress])
      , (evi == evi2 || evi `elem` [Express UEVI, NoExpress])
      , (tra == tra2 || tra `elem` [Express UTRA, NoExpress])
      , (vol == vol2 || vol `elem` [Express UVOL, NoExpress])
      ]

translateLeaf :: [[Phoneme]] -> [((Text, LexCat), Morpheme)] -> Leaf -> Text
translateLeaf _ _ LeafNull{} = ""
translateLeaf sonHier dict (Leaf lc _ str) = translate sonHier dict (str,lc)
translateLeaf _ _ _ = "ERROR"

translate :: [[Phoneme]] -> [((Text, LexCat), Morpheme)] -> (Text, LexCat) -> Text
translate sonHier dict ent = fromMaybe "<UNK>" (parseMorphemeIPA sonHier <$> lookup ent dict)

-- to romanized
romanizeLeaf :: [((Text, LexCat), Morpheme)] -> Leaf -> Text
romanizeLeaf _ LeafNull{} = ""
romanizeLeaf dict (Leaf lc _ str) = fromMaybe "<UNK>" (romanizeMorpheme <$> lookup (str, lc) dict)
romanizeLeaf _ _ = "ERROR"

romanizeLeaves :: Grammar -> [((Text, LexCat), Morpheme)] -> [ManifestSystem] -> [[Leaf]] -> Text
romanizeLeaves g dict infl leaves = unwords $ map (romanizeLeaves2 g dict infl) leaves

romanizeLeaves2 :: Grammar -> [((Text, LexCat), Morpheme)] -> [ManifestSystem] -> [Leaf] -> Text
romanizeLeaves2 g dict infl leaves
  | any leafIsInfl leaves = romanizeInfl g dict infl leaves
  | otherwise             = concatMap (romanizeLeaf dict) leaves

romanizeInfl :: Grammar -> [((Text, LexCat), Morpheme)] -> [ManifestSystem] -> [Leaf] -> Text
romanizeInfl g dict infl leaves = out where
  (others, inflLeaves) = break leafIsInfl leaves
  infls = map leafInfl inflLeaves

  -- retrieve the relevent particles/prefixes/suffixes from the manifest systems
  minfl = filter (\x -> fromMaybe False ((==) (manSysLC x) . leafLC <$> listToMaybe inflLeaves)) infl
  mparts = filter (\x -> manSysType x == Particle) minfl
  mprefs = filter (\x -> manSysType x == Prefix) minfl
  msuffs = filter (\x -> manSysType x == Prefix) minfl
  partCombos = map manSysCombos mparts
  prefCombos = map manSysCombos mprefs
  suffCombos = map manSysCombos msuffs
  parts = concatMap (getLast . map (fst . snd) . filter (\(j,(x,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) partCombos
  prefs = concatMap (getLast . map (fst . snd) . filter (\(j,(x,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) prefCombos
  suffs = concatMap (getLast . map (fst . snd) . filter (\(j,(x,i)) ->  (compareInfl i j)) . ((,) <$> infls <*>)) suffCombos

  -- syllabify the output properly
  partOut = map (romanizeWord . Word . (:[])) parts
  othersOut = map (\x -> case x of (Leaf lc _ str) -> fromMaybe ((romanizeWord . Word) suffs ++ "<UNK>" ++ (romanizeWord . Word) prefs) ((romanizeWord . Word) <$> ((++ suffs) . (prefs ++) . (:[]) <$> lookup (str, lc) dict))
                                   (LeafNull _) -> "") others

  out
    | null others          = unwords partOut
    | otherwise            = unwords othersOut

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
  dHeadOut = [[dHead]]
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
    | getWHM g == OblWHMove && phraseIl vComp == Ques && phraseLC vComp == Det  = [[Leaf Noun Ques "Who/what"]]
    | getWHM g == OblWHMove && phraseIl vComp == Ques && phraseLC vComp == Adpo = [[Leaf Noun Ques "Where"]]
    | otherwise                                                                 = parsePhrase g cSpec
  cHeadOut
    | getItoC g == OblItoCMove && cHead == LeafNull Ques = [[iHead]]
    | otherwise                                          = [[cHead]]
  iSpecOut
    | True      = parsePhrase g vSpec
    | otherwise = parsePhrase g iSpec
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
    | otherwise = parsePhrase g vSpec
  vHeadOut
    | getVtoI g == OblVtoIMove                                = []
    | (getVtoI g == NoVtoIMove && getAH g == OblAffixHop)
      && (getItoC g /= OblItoCMove || cHead /= LeafNull Ques) = [[vHead, iHead]]
    | otherwise                                               = [[vHead]]
  vCompOut
    | getWHM g == OblWHMove && phraseIl vComp == Ques = []
    | otherwise                                       = parsePhrase g vComp

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
