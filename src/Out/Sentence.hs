module Out.Sentence
( writeParseTree
, writePhrase
, compareInfl
) where

import ClassyPrelude hiding (Word)

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
writeParseTree :: Language -> [Morpheme] -> [Morpheme] -> Phrase -> Text
writeParseTree lang rootMorphs inflMorphs pt = "\n<br>\n<br>\n" ++ table ++ "<br>\n" ++ literal ++ "\n<br>\n\"" ++ english ++ "\"" where
  leaves   = filter (not.all leafIsNull) (filter (not.null) (writePhrase lang pt))
  eLeaves  = filter (not.all leafIsNull) (filter (not.null) (writePhrase englishLanguage pt))
  -- native = ...
  table    = tableLeaves lang rootMorphs inflMorphs leaves
  literal  = translateLeaves leaves
  english  = translateLeaves eLeaves

-- This makes a table so romanization, ipa, and gloss all line up
tableLeaves :: Language -> [Morpheme] -> [Morpheme] -> [[Leaf]] -> Text
tableLeaves lang rootMorphs inflMorphs leaves = "\n<table border=1>" ++ tHeader ++ romanRow ++ ipaRow ++ glossRow ++ "\n</table>\n" where
  tHeader = "\n\t<tr>\n\t\t<th colspan=\"" ++ tshow (length leaves + 1) ++ "\">Example</th>\n\t</tr>"

  romanRow = "\n\t<tr>\n\t\t<th>" ++ "Romanized" ++ "</th>" ++ romanCluster ++ "\n\t</tr>"
  romanCluster = "\n\t\t<td>" ++ intercalate "</td>\n\t\t<td>" roman ++ "</td>"
  roman = map (romanizeLeaves lang rootMorphs inflMorphs) leaves

  ipaRow = "\n\t<tr>\n\t\t<th>" ++ "Transcribed" ++ "</th>" ++ ipaCluster ++ "\n\t</tr>"
  ipaCluster = "\n\t\t<td>" ++ intercalate "</td>\n\t\t<td>" ipa ++ "</td>"
  ipa = map (transcribeLeaves lang rootMorphs inflMorphs) leaves

  glossRow = "\n\t<tr>\n\t\t<th>" ++ "Glossed" ++ "</th>" ++ glossCluster ++ "\n\t</tr>"
  glossCluster = "\n\t\t<td>" ++ intercalate "</td>\n\t\t<td>" gloss ++ "</td>"
  gloss = map (glossLeaves inflMorphs) leaves

leafIsInfl :: Leaf -> Bool
leafIsInfl LeafInfl{} = True
leafIsInfl _ = False

leafIsNull :: Leaf -> Bool
leafIsNull LeafNull{} = True
leafIsNull _ = False

-- Gloss
glossLeaves :: [Morpheme] -> [Leaf] -> Text
glossLeaves inflMorphs leaves = out where
  (others, inflLeaves)  = break leafIsInfl leaves
  infls                 = map leafInfl inflLeaves

  (partMorphs, prefMorphs, suffMorphs, transMorphs) = getReleventInflMorphs inflMorphs inflLeaves
  parts = filter (\x -> any (compareInfl (getAllExpress $ getMeaning x)) infls) partMorphs
  prefs = filter (\x -> any (compareInfl (getAllExpress $ getMeaning x)) infls) prefMorphs
  suffs = filter (\x -> any (compareInfl (getAllExpress $ getMeaning x)) infls) suffMorphs
  transs = filter (\x -> any (compareInfl (getAllExpress $ getMeaning x)) infls) transMorphs
  partsOut = map (glossCombo . getAllExpress . getMeaning) parts
  prefsOut = map (glossCombo . getAllExpress . getMeaning) prefs
  suffsOut = map (glossCombo . getAllExpress . getMeaning) suffs
  transsOut = map (glossCombo . getAllExpress . getMeaning) transs

  out
    | null others = unwords partsOut -- not sure why it does this
    | null inflLeaves = concatMap translateLeaf leaves
    | otherwise   = intercalate "-" prefsOut ++ (if null prefs then "" else "-") ++ unwords (map translateLeafWithDo others) ++ (if null transs then "" else "\\") ++ intercalate "-" transsOut ++ (if null suffs then "" else "-") ++ intercalate "-" suffsOut

-- Get the relevent inflection morphemes from the "master" list
-- Also sorts them by type, particle, prefix, suffix
getReleventInflMorphs :: [Morpheme] -> [Leaf] -> ([Morpheme], [Morpheme], [Morpheme], [Morpheme])
getReleventInflMorphs _ []  = ([],[],[],[])
getReleventInflMorphs inflMorphs inflLeaves = (partMorphs, prefMorphs, suffMorphs, transMorphs) where
  lc = leafLC $ unsafeHead inflLeaves
  partMorphs = filter (\m -> getMorphType m == Particle && getLC (getMeaning m) == lc) inflMorphs
  prefMorphs  = filter (\m -> getMorphType m == Prefix && getLC (getMeaning m) == lc) inflMorphs
  suffMorphs = filter (\m -> getMorphType m == Suffix && getLC (getMeaning m) == lc) inflMorphs
  transMorphs = filter (\m -> getMorphType m == Transfix && getLC (getMeaning m) == lc) inflMorphs

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
glossCombo_ x
  | x /= NoExpress = gloss (getExp x)
  | otherwise = ""

-- Transcribe sentence into IPA
transcribeLeaves :: Language -> [Morpheme] -> [Morpheme] -> [Leaf] -> Text
transcribeLeaves lang rootMorphs inflMorphs leaves = out where
  (others, inflLeaves)  = break leafIsInfl leaves
  infls                 = map leafInfl inflLeaves

  (partMorphs, prefMorphs, suffMorphs, transMorphs) = getReleventInflMorphs inflMorphs inflLeaves
  parts = filter (\x -> any (compareInfl (getAllExpress $ getMeaning x)) infls) partMorphs
  prefs = filter (\x -> any (compareInfl (getAllExpress $ getMeaning x)) infls) prefMorphs
  suffs = filter (\x -> any (compareInfl (getAllExpress $ getMeaning x)) infls) suffMorphs
  transs = filter (\x -> any (compareInfl (getAllExpress $ getMeaning x)) infls) transMorphs

  partOut = map (writeWordIPA lang) parts

  othersOut = map (\x -> case x of Leaf{} -> fromMaybe (concatMap (writeMorphemeIPA lang) suffs ++ "<UNK>" ++ concatMap (writeMorphemeIPA lang) prefs) (writeWordIPA lang <$> join ((\z -> foldlM applyMorpheme z (transs ++ suffs ++ prefs)) <$> find (\y -> RootMeaning (leafLC x) (leafStr x) == getMeaning y) rootMorphs))
                                   LeafNull{} -> ""
                                   _ -> ""
                                   ) others

  out
    | null others          = unwords partOut -- again, not sure why this
    | null inflLeaves      = concatMap (transcribeLeaf lang rootMorphs) leaves
    | otherwise            = unwords othersOut

getLast :: [a] -> [a]
getLast [] = []
getLast xs = fromMaybe [] ((:[]) <$> lastMay xs)

-- Compares new language infl (1) to English infl (2)
-- Actually this checks if the left GramCatExpress helps satisfy the right GramCatExpress
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
compareInfl_ x y = x == y || x `elem` [Express minBound, NoExpress]

transcribeLeaf :: Language -> [Morpheme] -> Leaf -> Text
transcribeLeaf _ _ LeafNull{} = ""
transcribeLeaf lang rootMorphs (Leaf lc _ str) = fromMaybe "<UNK>" (writeMorphemeIPA lang <$> find (\x -> RootMeaning lc str == getMeaning x) rootMorphs)
transcribeLeaf _ _ _ = "ERROR"

-- romanize
romanizeLeaves :: Language -> [Morpheme] -> [Morpheme] -> [Leaf] -> Text
romanizeLeaves lang rootMorphs inflMorphs leaves
  | any leafIsInfl leaves = romanizeInfl lang rootMorphs inflMorphs leaves
  | otherwise             = concatMap (romanizeLeaf lang rootMorphs) leaves

romanizeInfl :: Language -> [Morpheme] -> [Morpheme] -> [Leaf] -> Text
romanizeInfl lang rootMorphs inflMorphs leaves = out where
  (others, inflLeaves)  = break leafIsInfl leaves
  infls                 = map leafInfl inflLeaves

  (partMorphs, prefMorphs, suffMorphs, transMorphs) = getReleventInflMorphs inflMorphs inflLeaves
  parts = filter (\x -> any (compareInfl (getAllExpress $ getMeaning x)) infls) partMorphs
  prefs = filter (\x -> any (compareInfl (getAllExpress $ getMeaning x)) infls) prefMorphs
  suffs = filter (\x -> any (compareInfl (getAllExpress $ getMeaning x)) infls) suffMorphs
  transs = filter (\x -> any (compareInfl (getAllExpress $ getMeaning x)) infls) transMorphs

  partOut = map (romanizeWord lang) parts

  othersOut = map (\x -> case x of Leaf{} -> fromMaybe (concatMap (romanizeWord lang) suffs ++ "<UNK>" ++ concatMap (romanizeWord lang) prefs) (romanizeWord lang <$> join ((\z -> foldlM applyMorpheme z (transs ++ suffs ++ prefs)) <$> find (\y -> Meaning (leafLC x) (leafStr x) (leafInfl x) == getMeaning y) rootMorphs))
                                   LeafNull{} -> ""
                                   _ -> ""
                                   ) others

  out
    | null others          = unwords partOut -- again, not sure why this
    | null inflLeaves      = concatMap (romanizeLeaf lang rootMorphs) leaves
    | otherwise            = unwords othersOut


romanizeLeaf :: Language -> [Morpheme] -> Leaf -> Text
romanizeLeaf _ _ LeafNull{} = ""
romanizeLeaf lang rootMorphs (Leaf lc _ str) = fromMaybe "<UNK>" (romanizeWord lang <$> find (\x -> RootMeaning lc str == getMeaning x) rootMorphs)
romanizeLeaf _ _ _ = "!!ERROR!!"


-- (Tries to) translate to English
translateLeaves :: [[Leaf]] -> Text
translateLeaves leaves = unwords $ map translateLeaves2 leaves

translateLeaves2 :: [Leaf] -> Text
translateLeaves2 leaves = out where
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
    | null inflLeaves = concatMap translateLeaf leaves
    | otherwise   = concat prefs ++ unwords (map translateLeafWithDo others) ++ concat suffs

translateLeaf :: Leaf -> Text
translateLeaf LeafNull{} = ""
translateLeaf (Leaf _ _ str) = str
translateLeaf _ = "ERROR"

-- Do insertion
translateLeafWithDo :: Leaf -> Text
translateLeafWithDo LeafNull{} = "do"
translateLeafWithDo (Leaf _ _ str) = str
translateLeafWithDo _ = "ERROR"


-- Linearizes a parse tree using the grammar specified
-- It outputs a list of [Leaf] where [Leaf] contains a Leaf and any relevent LeafInfl's
-- Definitely not as elegant as it should be
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
