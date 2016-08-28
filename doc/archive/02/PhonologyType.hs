module PhonologyType
( 
VCTag(..)
--, VCSet(..)
, Letter(..)
, Alphabet
, Grapheme(..)
, GraphemeInventory
, Phone(..)
, PhoneInventory
, Phoneme(..)
, PhonemeInventory
, Onset(..)
, Nucleus(..)
, Coda(..)
, Syllable(..)
, Meaning(..)
, Relationship(..)
--, Morpheme(..)
, Word(..)
, DictionaryEntry(..)
, Dictionary(..)
, Language(..)
) where


import Prelude hiding (Word)
import Data.List

--data VCTag a = Vowel a | Consonant a 
data VCSet a = VCSet [a] [a] deriving (Show)

data Letter = Letter Char deriving (Eq, Ord, Show)
type Alphabet = VCSet Letter

data Grapheme = Grapheme [Letter] deriving (Eq, Ord, Show)
type GraphemeInventory = VCSet Grapheme


data Phone = Phone [Char] deriving (Eq, Ord, Show)
type PhoneInventory = VCSet Phone

data Phoneme = Phoneme [Phone] deriving (Eq, Ord, Show)
type PhonemeInventory = VCSet Phoneme

data GraphemePhoneme = GraphemePhoneme Grapheme Phoneme deriving (Eq, Ord, Show)
type GraphemePhonemeInventory = VCSet GraphemePhoneme

data Onset = Onset [GraphemePhoneme] deriving (Show)
data Nucleus = Nucleus GraphemePhoneme deriving (Show)
data Coda = Coda [GraphemePhoneme] deriving (Show)
data Syllable = Syllable Onset Nucleus Coda deriving (Show)

data POS = Noun | Verb | Adjective | Adverb deriving (Show)

data Meaning = Meaning [Char] deriving (Show)
data Relationship = Relationship Meaning [Meaning] deriving (Show)

--data Morpheme = Morpheme [Syllable] EnglishMeaning deriving (Show)

data Word = Word [Syllable] deriving (Show)

data DictionaryEntry = DictionaryEntry Word Meaning deriving (Show)

data Dictionary = Dictionary [DictionaryEntry] deriving (Show)

data Language = Language Dictionary deriving (Show)






