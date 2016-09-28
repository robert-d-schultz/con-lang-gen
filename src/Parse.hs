module Parse
( parseDictionary
, parseConPhonemeInventory
, parseVowPhonemeInventory
, parseConPhonotactics
, parseVowPhonotactics
) where

import Prelude hiding (Word)
import PhonemeType
import WordGen
import OtherData
import Data.List

-- Parse list of words to string
parseDictionary :: [Word] -> String
parseDictionary wrds = intercalate "\n" (map parseWord wrds)

-- Parse Word to string
parseWord :: Word -> String
parseWord (Word sylls) = "/" ++ intercalate "." (map parseSyllable sylls) ++ "/"

-- Parse Syllable to string
parseSyllable :: Syllable -> String
parseSyllable (Syllable cons1 vow cons2) = parseConsonant cons1 ++ parseVowel vow ++ parseConsonant cons2

-- Parse Consonant to string
parseConsonant :: MaybeConsonant -> String
parseConsonant Blank = []
parseConsonant (Consonant _ _ _ ipa) = ipa

-- Parse Vowel to string
parseVowel :: Vowel -> String
parseVowel (Vowel _ _ _ ipa) = ipa

--Parse the consonant inventory, should be a table in the final
parseConPhonemeInventory :: [MaybeConsonant] -> String
parseConPhonemeInventory cons = "Consonant inventory: " ++ intercalate ", " (init fList) ++ ", and " ++ last fList ++ "\n\n" where
  fList = filter (not . null) (map parseConsonant cons)

--Parse the consonant inventory, should be a table in the final
parseVowPhonemeInventory :: [Vowel] -> String
parseVowPhonemeInventory vows = "Vowel inventory: " ++ intercalate ", " (init fList) ++ ", and " ++ last fList ++ "\n\n" where
  fList = filter (not . null) (map parseVowel vows)

-- Parse phonotactics to some output form
parseConPhonotactics :: ([MaybeConsonant], [MaybeConsonant]) -> String
parseConPhonotactics (onset, coda) = "Syllable structure is: " ++ sBlurb1 ++ "V" ++ sBlurb2 ++ ".\n" ++ oBlurb ++ cBlurb ++ "\n" where

  sBlurb1
    | null onset || onset == [Blank] = []
    | Blank `elem` onset             = "(C)"
    | otherwise                      = "C"
  sBlurb2
    | null coda || coda == [Blank]   = []
    | Blank `elem` coda              = "(C)"
    | otherwise                      = "C"

  oBlurb
    | null onset || (onset == [Blank]) = "No consonants are allowed syllable-initial.\n"
    | Blank `elem` onset = "The following consonant(s) are allowed syllable-initial: /" ++ intercalate "/, /" (filter (not . null) (map parseConsonant onset)) ++ "/.\nSyllables may be vowel-initial.\n"
    | otherwise  = "The following consonant(s) are allowed syllable-initial: /" ++ intercalate "/, /" (filter (not . null) (map parseConsonant onset)) ++ "/.\n"
  cBlurb
    | null coda || (coda == [Blank]) = "No consonants are allowed syllable-final.\n"
    | Blank `elem` coda = "The following consonant(s) are allowed syllable-final: /" ++ intercalate "/, /" (filter (not . null) (map parseConsonant coda)) ++ "/.\n"
    | otherwise  = "The following consonant(s) are allowed syllable-final: /" ++ intercalate "/, /" (filter (not . null) (map parseConsonant coda)) ++ "/.\n"

parseVowPhonotactics :: ([Vowel], [Vowel], [Vowel], [Vowel]) -> String
parseVowPhonotactics (nucleus, onset, coda, alone) = concat [nBlurb, oBlurb, cBlurb, aBlurb] ++ "\n" where
  nBlurb
    | null nucleus = []
    | otherwise    = "The following vowels are allowed interconsonantal: /" ++ intercalate "/, /" (map parseVowel nucleus) ++ "/.\n"
  oBlurb
    | null onset   = []
    | otherwise    = "The following vowels are allowed syllable-initial: /" ++ intercalate "/, /" (map parseVowel onset) ++ "/.\n"
  cBlurb
    | null coda    = []
    | otherwise    = "The following vowels are allowed syllable-final: /" ++ intercalate "/, /" (map parseVowel coda) ++ "/.\n"
  aBlurb
    | null alone   = []
    | otherwise    = "The following vowels are allowed standing alone: /" ++ intercalate "/, /" (map parseVowel alone) ++ "/.\n"
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
