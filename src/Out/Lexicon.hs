module Out.Lexicon
( writeDictionary
, applyLemma
, applyMorpheme
, writeWordIPA
, writeMorphemeIPA
, writePhonemeIPA
, writeSyllablesIPA
, writeSyllableIPA
, writeLC
) where

import ClassyPrelude hiding (Word)
import GHC.Exts (groupWith)

import Data.Phoneme
import Data.Word
import Data.Inflection
import Data.Language

import Out.Roman
import Out.Syllable
import Out.IPA
import Out.Grapheme

import HelperFunctions

-- Write list of roots to string using a lemma
writeDictionary :: Language -> [(Phoneme, Text)] -> [Morpheme] -> [Morpheme] -> [Morpheme] -> [Morpheme] -> Text
writeDictionary lang ndict rootMorphs lemmaMorphs prons inflMorphs = out where
  ws = map (applyLemma lemmaMorphs) (rootMorphs ++ prons)
  particles = filter (\x -> Particle == getMorphType x) inflMorphs
  ws_p = ws ++ map Just particles
  out = "\n" ++ intercalate "\n" (map (writeDictionaryEntry lang ndict) (reduceHomophones lang ws_p))

applyLemma :: [Morpheme] -> Morpheme -> Maybe Word
applyLemma lemmaMorphs root = foldlM applyMorpheme root (transfixes ++ ctransfixes ++ suffixes ++ prefixes) where
  lemmaMorphs_ = filter (\x -> (getLC.getMeaning) root == (getLC.getMeaning) x) lemmaMorphs
  prefixes     = filter ((\case Prefix    -> True; _ -> False) . getMorphType) lemmaMorphs_
  suffixes     = filter ((\case Suffix    -> True; _ -> False) . getMorphType) lemmaMorphs_
  transfixes   = filter ((\case Transfix  -> True; _ -> False) . getMorphType) lemmaMorphs_
  ctransfixes  = filter ((\case CTransfix -> True; _ -> False) . getMorphType) lemmaMorphs_

applyMorpheme :: Word -> Morpheme -> Maybe Word
applyMorpheme word morpheme = do
  -- Meaning composition
  meaningN <- composeMeaning (getMeaning word) (getMeaning morpheme)
  -- Order
  return $
    if | getMorphType morpheme == Prefix -> Word meaningN morpheme word
       | getMorphType morpheme == Suffix -> Word meaningN word morpheme
       | otherwise                       -> Word meaningN word morpheme

composeMeaning :: Meaning -> Meaning -> Maybe Meaning
-- Root meaning + compound meaning = partial (compound) meaning
composeMeaning (RootMeaning lc1 str1) (CompMeaning lc2 lc21 lc22 str2) = out where
  out
    | lc1 /= lc21 = Nothing
    | otherwise = Just $ CompMeaning lc2 lc21 lc22 (format str2 [str1, "{0}"])
-- Partial (compound) meaning + root meaning = meaning
composeMeaning (CompMeaning lc1 lc11 lc12 str1) (RootMeaning lc2 str2) = out where
  out
    | lc12 /= lc2 = Nothing
    | otherwise = Just $ Meaning lc1 (format str1 [str2]) gramCatExpressNull
-- Root meaning + derivation meaning = meaning
composeMeaning (RootMeaning lc1 str1) (DeriMeaning lc21 lc22 str2) = out where
  out
    | lc1 /= lc21 = Nothing
    | otherwise = Just $ Meaning lc22 (str2 ++ str1) gramCatExpressNull
-- Meaning + derivation meaning = meaning
composeMeaning (Meaning lc1 str1 infl1) (DeriMeaning lc21 lc22 str2) = out where
  out
    | lc1 /= lc21 = Nothing
    | otherwise = Just $ Meaning lc22 (str2 ++ str1) infl1
-- Root meaning + inflection meaning = meaning
composeMeaning (RootMeaning lc1 str) (InflMeaning lc2 infl) = out where
  out
    | lc1 /= lc2 = Nothing
    | otherwise = Just $ Meaning lc1 str infl
-- Meaning + inflection meaning = meaning
composeMeaning (Meaning lc1 str infl1) (InflMeaning lc2 infl2) = out where
  out
    | lc1 /= lc2 = Nothing
    | otherwise = Meaning lc1 str <$> inflN
  inflN = combineGramCatExpress infl1 infl2
-- Anything else = crash
composeMeaning _ _ = Nothing

combineGramCatExpress :: GramCatExpress -> GramCatExpress -> Maybe GramCatExpress
combineGramCatExpress (GramCatExpress gen ani cas num def spe top per hon pol ten asp moo voi evi tra vol) (GramCatExpress gen2 ani2 cas2 num2 def2 spe2 top2 per2 hon2 pol2 ten2 asp2 moo2 voi2 evi2 tra2 vol2) = out where
  out = GramCatExpress <$> combineGCE_ gen gen2
                       <*> combineGCE_ ani ani2
                       <*> combineGCE_ cas cas2
                       <*> combineGCE_ num num2
                       <*> combineGCE_ def def2
                       <*> combineGCE_ spe spe2
                       <*> combineGCE_ top top2
                       <*> combineGCE_ per per2
                       <*> combineGCE_ hon hon2
                       <*> combineGCE_ pol pol2
                       <*> combineGCE_ ten ten2
                       <*> combineGCE_ asp asp2
                       <*> combineGCE_ moo moo2
                       <*> combineGCE_ voi voi2
                       <*> combineGCE_ evi evi2
                       <*> combineGCE_ tra tra2
                       <*> combineGCE_ vol vol2

combineGCE_ :: Eq a => Bounded a => Express a -> Express a -> Maybe (Express a)
combineGCE_ x y
  | x == y = Just x
  | x `elem` [Express minBound, NoExpress] && y `elem` [Express minBound, NoExpress] = Just NoExpress -- not sure about this
  | x `elem` [Express minBound, NoExpress] = Just y
  | y `elem` [Express minBound, NoExpress] = Just x
  | otherwise = Nothing

reduceHomophones :: Language -> [Maybe Word] -> [(Maybe [Syllable], [Maybe Word])]
reduceHomophones lang ws = out where
  sylled = map (\w -> (join $ syllabifyWord lang <$> w, w) ) ws
  sorted = sortWith fst sylled
  grouped = groupWith fst sorted
  out = map (first (join . listToMaybe) . unzip) grouped


writeDictionaryEntry :: Language -> [(Phoneme, Text)] -> (Maybe [Syllable], [Maybe Word]) -> Text
writeDictionaryEntry lang ndict (sylls, ws) = "<br>\n<br>\n"
                                            -- ++ writeMorphemeNative morph ndict ++ " "
                                            ++ "<i>" ++ fromMaybe "!!ERROR!!" (romanizeSyllables lang <$> sylls <*> return []) ++ "</i>"
                                            ++ " (" ++ fromMaybe "!!ERROR!!" (writeSyllablesIPA <$> sylls) ++ ")"
                                            ++ concatMap (\x -> "\n<br>\n\t&emsp;" ++ fromMaybe "!!ERROR!!" ( do
                                                                                                                y <- x
                                                                                                                return ((writeMeaning (getMorphType y) . getMeaning) y)
                                                                                                             )
                                                         ) ws
writeMeaning :: MorphType -> Meaning -> Text
writeMeaning _ (Meaning Pron _ gce) = writeAllExpress gce ++ " pronoun."
writeMeaning _ (Meaning lc str _) = writeLC lc ++ " " ++ str ++ "." -- ++ ", inflected for " ++ writeAllExpress gce ++ "."
writeMeaning _ (RootMeaning lc str) = writeLC lc ++ " " ++ str ++ "."
writeMeaning mt (InflMeaning lc gce) = writeAllExpress gce ++ " " ++ tshow mt ++ " that inflects " ++ tshow lc ++ "s."
writeMeaning mt (DeriMeaning lc1 lc2 str) = "derivational " ++ tshow mt ++ " that changes a " ++ tshow lc1 ++ " into " ++ tshow lc2 ++ " using \"" ++ str ++ " <" ++ tshow lc1 ++ ">\"."
writeMeaning mt (CompMeaning lc lc1 lc2 str) = "compound " ++ tshow mt ++ " that combines a " ++ tshow lc1 ++ " and " ++ tshow lc2 ++ "into a " ++ tshow lc ++ " using \"" ++ format str ["<" ++ tshow lc1 ++ ">", "<" ++ tshow lc2 ++ ">"] ++ "\"."

writeAllExpress :: GramCatExpress-> Text
writeAllExpress gce = out where
  filt = filter (not.null) a
  out = case filt of [x]   -> x
                     [x,y] -> x ++ ", " ++ y
                     _     -> fromMaybe "" (do
                                              i <- initMay filt
                                              l <- lastMay filt
                                              return $ intercalate ", " i ++ ", " ++ l
                                           )
  a = [ tshow $ getGen gce
      , tshow $ getAni gce
      , tshow $ getCas gce
      , tshow $ getNum gce
      , tshow $ getDef gce
      , tshow $ getSpe gce
      , tshow $ getTop gce
      , tshow $ getPer gce
      , tshow $ getHon gce
      , tshow $ getPol gce
      , tshow $ getTen gce
      , tshow $ getAsp gce
      , tshow $ getMoo gce
      , tshow $ getVoi gce
      , tshow $ getEvi gce
      , tshow $ getTra gce
      , tshow $ getVol gce
      ]


writeLC :: LexCat -> Text
writeLC lc
  | lc == Verb = "v."
  | lc == Noun = "n."
  | lc == Adj  = "adj."
  | lc == Adv  = "adv."
  | lc == Adpo = "p."
  | otherwise = ""

-- write Word to string
writeWordIPA :: Language -> Word -> Text
writeWordIPA lang word = fromMaybe "!!Word doesn't syllabize!!" out where
  out = do
    sylls <- syllabifyWord lang word
    return $ "/" ++ intercalate "." (map writeSyllableIPA sylls) ++ "/"

-- write Morpheme to string (used in exponent table too)
writeMorphemeIPA :: Language -> Morpheme -> Text
writeMorphemeIPA lang (MorphemeS _ _ sylls) = "/" ++ writeSyllablesIPA sylls ++ "/"
writeMorphemeIPA lang m@(MorphemeP _ _ ps) = fromMaybe ("!!Morpheme doesn't syllabize!! /" ++ concatMap writePhonemeIPA ps ++"/") out where
  out = do
    sylls <- syllabifyWord lang m
    return $ "/" ++ writeSyllablesIPA sylls ++ "/"
writeMorphemeIPA lang m@(MorphemeC _ _ pss) = "/" ++ intercalate "–" (map (concatMap writePhonemeIPA) pss) ++ "/"
writeMorphemeIPA lang m@(MorphemeV _ _ patts) = "/" ++ intercalate "–" (map writeSyllableIPA patts) ++ "/"

-- write Syllables to string
writeSyllablesIPA :: [Syllable] -> Text
writeSyllablesIPA [] = ""
writeSyllablesIPA [syll] = writeSyllableIPA syll
writeSyllablesIPA (syll@(Syllable _ _ _ _ NONES):sylls) = writeSyllableIPA syll ++ "." ++ writeSyllablesIPA sylls
writeSyllablesIPA (syll:sylls) = writeSyllableIPA syll ++ writeSyllablesIPA sylls

-- write Syllable to string
writeSyllableIPA :: Syllable -> Text
writeSyllableIPA (Syllable onset nuc@Consonant{} coda tone stress) = concatMap writePhonemeIPA onset
                                                                  ++ writePhonemeIPA nuc
                                                                  ++ "\809"
                                                                  ++ concatMap writePhonemeIPA coda
                                                                  ++ writeToneLetterIPA tone
                                                                  ++ writeStressMarkIPA stress
writeSyllableIPA (Syllable onset nucleus coda tone stress) = concatMap writePhonemeIPA onset
                                                          ++ writePhonemeIPA nucleus
                                                          ++ concatMap writePhonemeIPA coda
                                                          ++ writeToneLetterIPA tone
                                                          ++ writeStressMarkIPA stress
