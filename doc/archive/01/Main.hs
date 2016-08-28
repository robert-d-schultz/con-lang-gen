-- ConLangGen VI (aka GlossaGenesis)
-- By: Robert "Brighty" Schultz, Linguist Extraordinaire
--     Patrick "Buppy", Haskell Master
-- 13 May 2015 CE
-- 4 Canterbury Court, Piscataway Township, 08854
-- 
-- 
-- The following program generates a constructed language.
-- 
-- Possibilies:
-- * A bilingual dictionary. (Lexis)
-- * A description of the phones used, as well as allophonic exceptions. (Phonology)
-- * Description and list of morphemes. (Morphology)
-- * Character set in .ttf format. (Graphemics)
-- * Rules for writing out the language. (Orthography)
-- * A language development tree. (Historical linguistics)
-- * Language families. (Comparative linguistics)
-- * Song and poems. (Prosody)

-- Import libraries
import Prelude hiding (Word)
import System.Random
import Control.Monad
import Data.Random hiding (sample)
import qualified Data.Random.Sample as Sampler
import Data.Random.Extras hiding (shuffle)
import Data.Random.Source.IO
import Data.Functor
import Data.List
import Control.Applicative
import System.IO

-- Data structures
import PhonologyType

-- Hack to get it working on Windows
writeUTF8File fp content =
  withFile fp WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h content

-- Program
-- Load from files 
data InputData = InputData
    { inputPhonesV :: [Phone]
    , inputPhonesC :: [Phone]
    , inputLettersV :: [Letter]
    , inputLettersC :: [Letter]
	, inputMeaningsN :: [Relationship]
    }

loadInputData :: IO InputData
loadInputData =
    InputData
        <$> readPhones "phonology/phonesV.txt"
        <*> readPhones "phonology/phonesC.txt"
        <*> readLetters "graphemics/lettersV.txt"
        <*> readLetters "graphemics/lettersC.txt"
		<*> readMeanings "lexis/meaningsN.txt"
    
readPhones :: FilePath -> IO [Phone]
readPhones fileName = do
    phones_text <- readFile fileName
    let phones_string = read phones_text :: [[[Char]]]
    let filtered = map (filter (/="\NUL")) phones_string
    return $ map Phone $ concat filtered
    
readLetters :: FilePath -> IO [Letter]
readLetters fileName = do
    letters_text <- readFile fileName
    let letters_char = read letters_text :: [Char]
    return $ map Letter letters_char
	
readMeanings :: FilePath -> IO [Relationship]
readMeanings fileName = do
    meanings_text <- readFile fileName
    let meanings_string = read meanings_text :: [[[Char]]]
    let heads = map head meanings_string
    let tails = map tail meanings_string
    let heads_meaning = map Meaning heads
    let tails_meaning = map (map Meaning) tails
    return $ zipWith Relationship heads_meaning tails_meaning
    
-- Make alphabet
makeAlphabet :: InputData -> Int -> Int -> RVar Alphabet
makeAlphabet inputD n m = do
    pickedV <- Sampler.sample (take n <$> shuffle (inputLettersV inputD))
    pickedC <- Sampler.sample (take m <$> shuffle (inputLettersC inputD))
    return $ VCSet pickedV pickedC

-- Make grapheme inventory
makeGraphemeInventory :: InputData -> Int -> Int -> Int -> Int -> RVar GraphemeInventory
makeGraphemeInventory inputD a b n m = do
    (VCSet v c) <- makeAlphabet inputD a b
    let seqV = subsequences v
    let seqC = subsequences c
    let shortenedV = filter (\str -> (length str > 1) && (length str <= 2)) seqV
    let shortenedC = filter (\str -> (length str > 1) && (length str <= 2)) seqC
    result1 <- Sampler.sample (take n <$> shuffle shortenedV)
    result2 <- Sampler.sample (take m <$> shuffle shortenedC)
    return $ VCSet (map Grapheme (result1 ++ (map (:[]) v))) (map Grapheme (result2 ++ (map (:[]) c)))

-- Make phone inventory
makePhoneInventory :: InputData -> Int -> Int -> RVar PhoneInventory
makePhoneInventory inputD n m = do
    pickedV <- Sampler.sample (take n <$> shuffle (inputPhonesV inputD))
    pickedC <- Sampler.sample (take m <$> shuffle (inputPhonesC inputD))
    return (VCSet pickedV pickedC)
    
-- Make phoneme inventory
makePhonemeInventory :: InputData -> Int -> Int -> Int -> Int -> RVar PhonemeInventory
makePhonemeInventory inputD a b c d = do
    (VCSet v c) <- makeGraphemeInventory inputD a b c d
    (VCSet v2 c2) <- makePhoneInventory inputD (length v) (length c)
    return $ VCSet (zipWith Phoneme v v2) (zipWith Phoneme c c2)

-- Make syllable
makeSyllable :: PhonemeInventory -> RVar Syllable
makeSyllable (VCSet v c) = do
    onset <- Sampler.sample (take 1 <$> shuffle c)
    nucleus <- Sampler.sample (take 1 <$> shuffle v)
    coda <- Sampler.sample (take 1 <$> shuffle c)
    let rime = Rime (Nucleus (head nucleus)) (Coda coda)
    let syll = Syllable (Onset onset) rime
    return syll
    
-- Make word
makeWord :: PhonemeInventory -> RVar Word
makeWord phonemes = do
    syllList <- replicateM 3 (makeSyllable phonemes)
    return $ Word syllList
    
-- Make dictionary
makeDictionary :: InputData -> Int -> Int -> Int -> Int -> RVar Dictionary
makeDictionary inputD a b n m = do
    phonemes <- makePhonemeInventory inputD a b n m
    let meaningList = map relationshipToMeaning (inputMeaningsN inputD)
    wordList <- replicateM (length meaningList) (makeWord phonemes)
    return $ Dictionary $ zipWith DictionaryEntry wordList meaningList

relationshipToMeaning :: Relationship -> Meaning
relationshipToMeaning relationship = meaning
    where (Relationship meaning _) = relationship

-- Dictionary text output
writeDictionary :: Dictionary -> IO ()
writeDictionary dict = do
    let Dictionary entryList = dict
    let contents = "Bilingual Dictonary\n" ++ (concat (intersperse "\n" (map makeDictionaryLine entryList)))
    writeUTF8File "dictionaryOutput.txt" contents
	
makeDictionaryLine :: DictionaryEntry -> [Char]
makeDictionaryLine entry = 
    let DictionaryEntry word meaning = entry
    in (wordToString word) ++ " ([" ++ (wordToIPA word) ++ "]) - " ++ (meaningToString meaning) ++ "."

meaningToString :: Meaning -> [Char]
meaningToString meaning = string
    where Meaning string = meaning
    
wordToString :: Word -> [Char]
wordToString word = 
    let syllList = wordToSyllables word
        phonemeList = concat (map syllToPhonemes syllList)
        graphemeList = map phonemeToGrapheme phonemeList
        letterList = concat (map graphemeToLetters graphemeList)
    in map letterToChar letterList

wordToIPA :: Word -> [Char]
wordToIPA word =
    let syllList = wordToSyllables word
        phonemeList = map syllToPhonemes syllList
        phoneList = map (map phonemeToPhone) phonemeList
        charList = map (map phoneToString) phoneList
        charList' = concat (concat (intersperse (".":[]) charList))
    in charList'

wordToSyllables :: Word -> [Syllable]
wordToSyllables word = syllList
    where Word syllList = word

syllToPhonemes :: Syllable -> [Phoneme]
syllToPhonemes syll = (onset ++ nucl:[] ++ coda)
    where Syllable (Onset onset) (Rime (Nucleus nucl) (Coda coda)) = syll

phonemeToPhone :: Phoneme -> Phone
phonemeToPhone phoneme = phone
    where Phoneme _ phone = phoneme
    
phonemeToGrapheme :: Phoneme -> Grapheme
phonemeToGrapheme phoneme = grapheme
    where Phoneme grapheme _ = phoneme
    
graphemeToLetters :: Grapheme -> [Letter]
graphemeToLetters grapheme = letterList
    where Grapheme letterList = grapheme

phoneToString :: Phone -> [Char]
phoneToString phone = string
    where Phone string = phone

letterToChar :: Letter -> Char
letterToChar letter = char
    where Letter char = letter

main :: IO ()
main = do
    inputD <- loadInputData
    result <- Sampler.sample (makeDictionary inputD 4 10 4 10)
    writeDictionary result
