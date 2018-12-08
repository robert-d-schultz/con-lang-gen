module Out.Lexicon
( writeDictionary
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

-- Write list of roots to string using a lemma
writeDictionary :: Language -> [(Phoneme, Text)] -> [Morpheme] -> LemmaMorphemes -> Text
writeDictionary lang ndict roots lemmas = out where
  ws = map (applyLemma lemmas) roots
  sylleds = map (\x -> fromMaybe [] . join $ syllabifyWord lang <$> x) ws
  meanings = map getMeaning roots
  out = "\n" ++ intercalate "\n" (map (writeDictionaryEntry lang ndict) (reduceHomophones (zip sylleds meanings)))

applyLemma :: LemmaMorphemes -> Morpheme -> Maybe Word
applyLemma lemmaMorphs root = do
  let lemmaMorphs_ = filter (\x -> (getLC.getMeaning) root == (getLC.getMeaning) x) lemmaMorphs
  let prefixes = filter ((\case InflMeaning _ Prefix _ -> True; _ -> False) . getMeaning) lemmaMorphs_
  let suffixes = filter ((\case InflMeaning _ Suffix _ -> True; _ -> False) . getMeaning) lemmaMorphs_
  let rest = filter ((\case InflMeaning _ Prefix _ -> False; InflMeaning _ Suffix _-> False; _ -> True) . getMeaning) lemmaMorphs
  if (not.null) rest then return $ Word (prefixes ++ [root] ++ suffixes) else Nothing

reduceHomophones :: [([Syllable], Meaning)] -> [([Syllable], [Meaning])]
reduceHomophones roots = map (first (fromMaybe [] . listToMaybe) . unzip) (groupWith fst (sortWith fst roots))


writeDictionaryEntry :: Language -> [(Phoneme, Text)] -> ([Syllable], [Meaning]) -> Text
writeDictionaryEntry lang ndict (sylls, meanings) = "<br>\n"
                                          --  ++ writeMorphemeNative morph ndict
                                            ++ " <i>" ++ concatMap romanizeSyllable sylls ++ "</i>"
                                            ++ " (" ++ writeSyllablesIPA sylls ++ ")"
                                            ++ concatMap (\(Meaning lc str) -> "\n<br>\t" ++ writeLC lc ++ " " ++ str) meanings

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
writeMorphemeIPA lang (MorphemeS _ sylls) = "/" ++ writeSyllablesIPA sylls ++ "/"
writeMorphemeIPA lang m@(MorphemeP _ ps) = fromMaybe ("!!Morpheme doesn't syllabize!! /" ++ concatMap writePhonemeIPA ps ++"/") out where
  out = do
    sylls <- syllabifyMorpheme lang m
    return $ "/" ++ writeSyllablesIPA sylls ++ "/"

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
