module Out.Lexicon
( writeDictionary
, writeWordIPA
, writeMorphemeIPA
, writePhonemeIPA
, writeSyllablesIPA
, writeSyllableIPA
, writeLC
, applyMorpheme
) where

import ClassyPrelude hiding (Word)
import GHC.Exts (groupWith)
import Text.Format

import Data.Phoneme
import Data.Word
import Data.Inflection
import Data.Language

import Out.Roman
import Out.Syllable
import Out.IPA
import Out.Grapheme

-- Write list of roots to string using a lemma
writeDictionary :: Language -> [(Phoneme, Text)] -> [Morpheme] -> [Morpheme] -> [Morpheme] -> Text
writeDictionary lang ndict rootMorphs lemmaMorphs inflMorphs = out where
  ws = map (applyLemma lemmaMorphs) rootMorphs
  particles = filter (\x -> Particle == getMorphType x) inflMorphs
  wsp = particles
  ws_ = ws ++ wsp
  sylleds = map (fromMaybe [] . syllabifyWord lang) ws_
  meanings = map getMeaning (rootMorphs ++ particles)
  out = "\n" ++ intercalate "\n" (map (writeDictionaryEntry lang ndict) (reduceHomophones (zip sylleds meanings)))

applyLemma :: [Morpheme] -> Morpheme -> Word
applyLemma lemmaMorphs root = foldl' applyMorpheme root (transfixes ++ ctransfixes ++ suffixes ++ prefixes) where
  lemmaMorphs_ = filter (\x -> (getLC.getMeaning) root == (getLC.getMeaning) x) lemmaMorphs
  prefixes = filter ((\case Prefix -> True; _ -> False) . getMorphType) lemmaMorphs_
  suffixes = filter ((\case Suffix -> True; _ -> False) . getMorphType) lemmaMorphs_
  transfixes = filter ((\case Transfix -> True; _ -> False) . getMorphType) lemmaMorphs_
  ctransfixes = filter ((\case CTransfix -> True; _ -> False) . getMorphType) lemmaMorphs_

applyMorpheme :: Word -> Morpheme -> Word
applyMorpheme word morpheme
  | getMorphType morpheme == Prefix = Word (getMeaning word) morpheme word
  | getMorphType morpheme == Suffix = Word (getMeaning word) word morpheme
  | otherwise = Word (getMeaning word) word morpheme

applyMorpheme_ :: Word -> Morpheme -> Maybe Word
applyMorpheme_ word morpheme = out where
  -- Meaning
  meaningN = combineMeaning (getMeaning word) (getMeaning morpheme)
  -- Order
  out
    | getMorphType morpheme == Prefix = Word <$> meaningN <*> return morpheme <*> return word
    | getMorphType morpheme == Suffix = Word <$> meaningN <*> return word <*> return morpheme
    | otherwise = Word <$> meaningN <*> return word <*> return morpheme

combineMeaning :: Meaning -> Meaning -> Maybe Meaning
combineMeaning (RootMeaning lc1 str1) (CompMeaning lc2 lc21 lc22 str2) = out where
  out
    | lc1 /= lc21 = Nothing
    | otherwise = Nothing
    -- | otherwise = Just $ CompMeaning lc2 lc21 lc22 (format str2 [str1, "{1}"])
combineMeaning (CompMeaning lc1 lc11 lc12 str1) (RootMeaning lc2 str2) = Nothing

combineMeaning (RootMeaning lc1 str1) (DeriMeaning lc21 lc22 str2) = out where
  out
    | lc1 /= lc21 = Nothing
    | otherwise = Just $ Meaning lc22 (str2 ++ str1) gramCatExpressNull
combineMeaning (Meaning lc1 str1 infl1) (DeriMeaning lc21 lc22 str2) = out where
  out
    | lc1 /= lc21 = Nothing
    | otherwise = Just $ Meaning lc22 (str2 ++ str1) infl1
combineMeaning (RootMeaning lc1 str) (InflMeaning lc2 infl) = out where
  out
    | lc1 /= lc2 = Nothing
    | otherwise = Just $ Meaning lc1 str infl
combineMeaning (Meaning lc1 str infl1) m2@(InflMeaning lc2 infl2) = out where
  out
    | lc1 /= lc2 = Nothing
    | otherwise = Meaning lc1 str <$> inflN
  inflN = combineGramCatExpress infl1 infl2
combineMeaning _ _ = Nothing

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

reduceHomophones :: [([Syllable], Meaning)] -> [([Syllable], [Meaning])]
reduceHomophones roots = map (first (fromMaybe [] . listToMaybe) . unzip) (groupWith fst (sortWith fst roots))


writeDictionaryEntry :: Language -> [(Phoneme, Text)] -> ([Syllable], [Meaning]) -> Text
writeDictionaryEntry lang ndict (sylls, meanings) = "<br>\n<br>\n"
                                            -- ++ writeMorphemeNative morph ndict ++ " "
                                            ++ "<i>" ++ concatMap romanizeSyllable sylls ++ "</i>"
                                            ++ " (" ++ writeSyllablesIPA sylls ++ ")"
                                            ++ concatMap (\x -> "\n<br>\n\t&emsp;" ++ writeMeaning x) meanings
writeMeaning :: Meaning -> Text
writeMeaning (Meaning lc str gce) = writeLC lc ++ " " ++ str ++ ", inflected for " ++ writeAllExpress gce ++ "."
writeMeaning (RootMeaning lc str) = writeLC lc ++ " " ++ str ++ "."
writeMeaning (InflMeaning lc gce) = writeAllExpress gce ++ " morpheme that inflects " ++ tshow lc ++ "s."
writeMeaning (DeriMeaning lc1 lc2 str) = "derivational morpheme that changes a " ++ tshow lc1 ++ " into " ++ tshow lc2 ++ " using \"" ++ str ++ " <" ++ tshow lc1 ++ ">\"."
-- writeMeaning (CompMeaning lc lc1 lc2 str) = "compound morpheme that combines a " ++ tshow lc1 ++ " and " ++ tshow lc2 ++ "into a " ++ tshow lc ++ " using \"" ++ format str ["<" ++ tshow lc1 ++ ">", "<" ++ tshow lc2 ++ ">"] ++ "\"."

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
