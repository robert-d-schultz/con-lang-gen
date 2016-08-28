module PhonologyGen
(makePhonemeInventory
, loadInputData
) where

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

-- Load from files 
data InputData = InputData
    { inputPhonesV :: [Phone]
    , inputPhonesC :: [Phone]
    , inputLettersV :: [Letter]
    , inputLettersC :: [Letter]
    }

loadInputData :: IO InputData
loadInputData =
    InputData
        <$> readPhones "phonology/phonesV.txt"
        <*> readPhones "phonology/phonesC.txt"
        <*> readLetters "graphemics/lettersV.txt"
        <*> readLetters "graphemics/lettersC.txt"
    
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
    
-- Make alphabet
makeAlphabet :: InputData -> Int -> Int -> RVar Alphabet
makeAlphabet inputD n m = do
    pickedV <- sample n (inputLettersV inputD)
    pickedC <- sample m (inputLettersC inputD)
    return $ VCSet pickedV pickedC

-- Make grapheme inventory
makeGraphemeInventory :: InputData -> Alphabet -> Int -> Int -> RVar GraphemeInventory
makeGraphemeInventory inputD alph n m = do
    let (VCSet v c) = alph
    let seqV = subsequences v
    let seqC = subsequences c
    let shortenedV = filter (\str -> (length str > 1) && (length str <= 2)) seqV
    let shortenedC = filter (\str -> (length str > 1) && (length str <= 2)) seqC
    result1 <- sample n shortenedV
    result2 <- sample m shortenedC
    return $ VCSet (map Grapheme (result1 ++ (map (:[]) v))) (map Grapheme (result2 ++ (map (:[]) c)))

-- Make phoneme inventory
-- Needs updating with allophones
-- Firzt need a change in the input structure of the phones csv
makePhonemeInventory :: InputData -> GraphemeInventory -> RVar PhonemeInventory
makePhonemeInventory inputD graph = do
    let (VCSet v c) = graph
    pickedV <- sample (length v) (inputPhonesV inputD)
    pickedC <- sample (length c) (inputPhonesC inputD)
    return $ VCSet pickedV pickedC
    
-- Make grapheme-phoneme inventory
makeGraphemePhonemeInventory :: InputData -> Int -> Int -> Int -> Int -> RVar GraphemePhonemeInventory
makeGraphemePhonemeInventory inputD a b c d = do
    alph <- makeAlphabet inputD a b
    graph <- makeGraphemeInventory inputD alph c d
    let VCSet v c = graph
    (VCSet v2 c2) <- makePhonemeInventory inputD graph
    return $ VCSet (zipWith GraphemePhoneme v v2) (zipWith GraphemePhoneme c c2)