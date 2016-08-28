-- Word Structure Generator

-- Import Libraries
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

-- Data
data SyllableStructure = CVC | VC | CV | V
data AvailableStructure = [SyllableStructure]
data NounStructure = NounStructure AvailableStructure (Int,[SyllableStructure]) (Int,[SyllableStructure]) (Int,[SyllableStructure])


--total unstructured
NounStructure avail (0,[]) (_,[]) (0,[])

--pick middle
NounStructure avail (0,[]) (_,foobar) (0,[]) 

--pick end, unstructured beg
NounStructure avail (0,[]) (_,[]) (1,bar)

--pick beg, unstructured end
NounStructure avail (1,foo) (_,[]) (0,[])

--pick beginning/end, unstructured middle
NounStructure avail (1,foo) (_,[]) (1,bar)

-- pick end and middle
NounStructure avail (0,[]) (_,foo) (1,bar)

-- pick beg and middle
NounStructure avail (1,foo) (_,bar) (0,[])

-- pick beg,end and middle
NounStructure avail (1,foobar) (_,foo) (1,bar)

-- total structured
NounStructure avail (0,[]) (1,foobar) (0,[]) 




-- Program
makeWordStructure :: Int -> PhonemeInventory -> RVar NounStructure
makeWordStructure thresh phonemeInventory = do
    avail <- Sampler.samplehead $ head $ shuffle $ tail $ subsequences [CVC ..]
    if length avail == 1
        num = random 1 2  
        if num == 1
            -- phoneInventory -> avail -> midNumber
            let midNum = ceiling (thresh / calcSyllStructComplex phonemeInventory (head avail))
            result = (0,[]) (midNum,[]) (0,[])
        if num == 2
            midVal = makeWordSubStruct avail thresh []
            result = (0,[]) (1,midVal) (0,[])
    if length avail > 1
        num = random 1 9    
        if num == 1
            if num' == 1
                -- phoneInventory -> midNumber
                result = (0,[]) (midNum,[]) (0,[])
            if num' == 2
                -- phoneInventory -> midVal -> midNumber
                result = (0,[]) (midNum,midVal) (0,[]) 
            if num' == 3
                -- phoneInventory -> endVal -> midNumber
                result = (0,[]) (midNum,[]) (1,endVal)
            if num' == 4
                -- phoneInventory -> begVal -> midNumber
                    result = (1,begVal) (midNum,[]) (0,[])
            if num' == 5
                -- phoneInventory -> begVal -> endVal -> midNumber
                result = (1,begVal) (midNum,[]) (1,endVal)
            if num' == 6
                -- phoneInventory -> midVal -> endVal -> midNumber
                result = (0,[]) (midNum,midVal) (1,endVal)
            if num' == 7
                -- phoneInventory -> begVal -> midVal -> midNumber
                result = (1,begVal) (midNum,midVal) (0,[])
            if num' == 8
                result = (1,begVal) (midNum,midVal) (1,endVal)
        if num == 2
            -- mid val needs to be constructed with phone inventory complexity in mind
            result = (0,[]) (1,midVal) (0,[])
        
    return NounStructure avail result

-- Word sub structure
makeWordSubStruct :: AvailableStructure -> Int -> [SyllableStructure] -> [SyllableStructure]
makeWordSubStruct avail thresh [] = do
    result <- Sampler.samplehead $ head $ shuffle avail
    return result:[]
makeWordSubStruct avail thresh substruct = do
    if (calcSubStructComplex substruct < thresh)
        result <- Sampler.samplehead $ head $ shuffle avail
        makeWordSubStruct avail thresh (substruct ++ result:[])
    if (calcSubStructComplex substruct >= thresh) 
        return substruct

-- Calc complexity
calcSyllStructComplex :: PhonemeInventory -> SyllableStructure -> Int
calcSyllStructComplex a CVC = 
calcSyllStructComplex a VC = 
calcSyllStructComplex a CV = 
calcSyllStructComplex a V = 
-- length of vowel graphemes
-- length of consonant graphemes

calcSubStructComplex :: PhonemeInventory -> [SyllableStructure] -> Int
calcSubStructComplex a b = 
    product (map (calcSyllStructComplex a) b)

--calcAvailStructComplex :: PhonemeInventory -> AvailableStructure -> Int
--calcAvailStructComplex a b = 
    --product (map (calcSyllStructComplex b) a)


-- Main
main :: IO ()
main = do
    inputD <- loadInputData
    result <- Sampler.sample (pickSystem inputD)
    print result