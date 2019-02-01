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
applyLemma lemmaMorphs root = foldl' applyMorpheme root (transfixes ++ ctransfixes ++suffixes ++ prefixes) where
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

reduceHomophones :: [([Syllable], Meaning)] -> [([Syllable], [Meaning])]
reduceHomophones roots = map (first (fromMaybe [] . listToMaybe) . unzip) (groupWith fst (sortWith fst roots))


writeDictionaryEntry :: Language -> [(Phoneme, Text)] -> ([Syllable], [Meaning]) -> Text
writeDictionaryEntry lang ndict (sylls, meanings) = "<br>\n<br>\n"
                                            -- ++ writeMorphemeNative morph ndict ++ " "
                                            ++ "<i>" ++ concatMap romanizeSyllable sylls ++ "</i>"
                                            ++ " (" ++ writeSyllablesIPA sylls ++ ")"
                                            ++ concatMap (\x -> "\n<br>\n\t&emsp;" ++ writeMeaning x) meanings
writeMeaning :: Meaning -> Text
writeMeaning (Meaning lc str) = writeLC lc ++ " " ++ str
writeMeaning (InflMeaning lc allExpress) = writeAllExpress allExpress ++ " particle that inflects " ++ tshow lc ++ "s"

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
writeMorphemeIPA lang m@(MorphemeC _ _ pss) = "/" ++ intercalate "-" (map (concatMap writePhonemeIPA) pss) ++ "/"
writeMorphemeIPA lang m@(MorphemeV _ _ patts) = "/" ++ intercalate "-" (map writeSyllableIPA patts) ++ "/"

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
