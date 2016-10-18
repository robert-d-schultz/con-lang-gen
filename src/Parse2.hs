module Parse2
( parseDictionary
, parseConPhonemeInventory
, parseVowPhonemeInventory
, parseDiphPhonemeInventory
, parseSonHier
) where

import Prelude hiding (Word)
import Data.List

import PhonemeType2
import WordGen2
import OtherData2


-- Parse list of words to string
parseDictionary :: [SyllWord] -> String
parseDictionary wrds = intercalate "\n" (map parseWord wrds)

-- Parse Word to string
parseWord :: SyllWord -> String
parseWord (SyllWord sylls) = "/" ++ intercalate "." (map parseSyllable sylls) ++ "/"

-- Parse Syllable to string
parseSyllable :: Syllable -> String
parseSyllable (Syllable onset (Consonant a b c d) coda) = concatMap parsePhoneme onset ++ parsePhoneme (Consonant a b c d) ++ "\809" ++ concatMap parsePhoneme coda
parseSyllable (Syllable onset nucleus coda) = concatMap parsePhoneme onset ++ parsePhoneme nucleus ++ concatMap parsePhoneme coda

-- Parse Phone to string
parsePhoneme :: Phoneme -> String
parsePhoneme Blank = []
parsePhoneme (Consonant _ _ _ ipa) = ipa
parsePhoneme (Vowel _ _ _ ipa) = ipa
parsePhoneme (Diphthong _ _ _ ipa1 _ _ _ ipa2) = ipa1 ++ ipa2

-- Parse the consonant inventory, should be a table in the final
parseConPhonemeInventory :: [Phoneme] -> String
parseConPhonemeInventory cons = "Consonant inventory: /" ++ intercalate "/, /" (init fList) ++ "/, and /" ++ last fList ++ "/\n\n" where
  fList = filter (not . null) (map parsePhoneme cons)

-- Parse the vowel inventory, should be a table in the final
parseVowPhonemeInventory :: [Phoneme] -> String
parseVowPhonemeInventory vows = "Vowel inventory: /" ++ intercalate "/, /" (init fList) ++ "/, and /" ++ last fList ++ "/\n\n" where
  fList = filter (not . null) (map parsePhoneme vows)

-- Parse the diphthong inventory, should be a table in the final
parseDiphPhonemeInventory :: [Phoneme] -> String
parseDiphPhonemeInventory diphs = "Diphthongs: /" ++ intercalate "/, /" (init fList) ++ "/, and /" ++ last fList ++ "/\n\n" where
  fList = filter (not . null) (map parsePhoneme diphs)

-- Parse the sonority hierarchy
parseSonHier :: [Phoneme] -> [[Phoneme]] -> String
parseSonHier vows cons = "Sonority hierarchy: " ++ "\n" ++ cListv ++ "\n" ++ intercalate "\n" cListc ++ "\n\n" where
  fListv = map parsePhoneme vows
  cListv = intercalate ", " fListv
  fListc = map (map parsePhoneme) cons
  cListc = map (intercalate ", ") fListc


{-
parseGrammarSystem :: GrammarSystem -> String
parseGrammarSystem gramSys = "Nouns are inflected in the following way:\n" ++
gen = fromMaybe [] $ gSys gramSys
  | M `elem` gen = "Masculine"
  | F `elem` gen = "Feminine"
  | COM `elem` gen = "Common"
  | N `elem` gen = "Neuter"
maybe [] id (aSys gramSys) = "Animacy"
maybe [] id (cSys gramSys) = "Case"
maybe [] id (nSys gramSys) = "Number"
maybe [] id (hSys gramSys) = "Honorific"
maybe [] id (dSys gramSys) = "Definiteness"
maybe [] id (sSys gramSys) = "Specificity"

parseDeclension :: Declension -> String
parseDeclension decl = "foob"
-}
