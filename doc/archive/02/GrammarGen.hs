-- Grammar Generator
module GrammarGen
(loadInputData2
, pickSystem
, makeDeclension
)
where

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

-- Data structures
import GrammarType
import PhonologyType

-- Program
-- Load from files 
data InputData = InputData
    { 
      inputGender :: [GenderSystem]
    , inputAnimacy :: [AnimacySystem]
    , inputCase :: [CaseSystem]
    , inputNumber :: [NumberSystem]
    , inputHonorific :: [HonorificSystem]    
    , inputDefiniteness :: [DefinitenessSystem]
    , inputSpecificity :: [SpecificitySystem]        
    }
    
loadInputData2 :: IO InputData
loadInputData2 =
    InputData
        <$> readFeature "grammar/syntactic/gender.txt"
        <*> readFeature "grammar/syntactic/animacy.txt"
        <*> readFeature "grammar/syntactic/case.txt"
        <*> readFeature "grammar/syntactic/number.txt"
        <*> readFeature "grammar/syntactic/honorific.txt"
        <*> readFeature "grammar/syntactic/definiteness.txt"
        <*> readFeature "grammar/syntactic/specificity.txt"
        
readFeature :: Read a => FilePath -> IO a
readFeature = fmap read . readFile
--readFeature = fmap (map GenderSystem . read) . readFile

-- Morphosyntactic features
pickSystem :: InputData -> RVar MorphosyntacticSystem
pickSystem inputD = MorphosyntacticSystem
    <$> pickGenderSystem inputD
    <*> pickAnimacySystem inputD
    <*> pickCaseSystem inputD
    <*> pickNumberSystem inputD
    <*> pickHonorificSystem inputD
    <*> pickDefinitenessSystem inputD
    <*> pickSpecificitySystem inputD

-- Gender system
pickGenderSystem :: InputData -> RVar GenderSystem
pickGenderSystem inputD =
    choice (inputGender inputD)

-- Animacy system
pickAnimacySystem :: InputData -> RVar AnimacySystem
pickAnimacySystem inputD = 
    choice (inputAnimacy inputD)

-- Case system
pickCaseSystem :: InputData -> RVar CaseSystem
pickCaseSystem inputD = 
    choice (inputCase inputD)

-- Number system
pickNumberSystem :: InputData -> RVar NumberSystem
pickNumberSystem inputD = 
    choice (inputNumber inputD)

-- Respect system
pickHonorificSystem :: InputData -> RVar HonorificSystem
pickHonorificSystem inputD = 
    choice (inputHonorific inputD)

-- Definiteness system
pickDefinitenessSystem :: InputData -> RVar DefinitenessSystem
pickDefinitenessSystem inputD = 
    choice (inputDefiniteness inputD)

-- Specificity system
pickSpecificitySystem :: InputData -> RVar SpecificitySystem
pickSpecificitySystem inputD = 
    choice (inputSpecificity inputD)
    
-- Make declension
makeDeclensionPattern :: MorphosyntacticSystem -> PhonemeInventory -> RVar DeclensionPattern
makeDeclensionPattern morph phonemes = do

    let MorphosyntacticSystem (GenderSystem a) (AnimacySystem b) (CaseSystem c) (NumberSystem d) (HonorificSystem e) (DefinitenessSystem f) (SpecificitySystem g) = morph

    --number of overall dimensions
    let lengths = [length a, length b, length c, length d, length e, length f, length g]
    let num = length (filter (> 0) lengths) 

    --pick number of "similar" dimensions
    let patterns = patternsGet num
    pat <- (choice patterns >>= choice)       
    nums <- sample 3 [0..6]
    let guide = (makeGuide nums [] pat)
    assignPhoneme guide morph phonemes

    where
        patternsGet a
            | a > 3 = 
                    [[totalRandom]
                    , [v, c, c2]
                    , [vc, cv, cc]
                    ]
            | a == 3 =
                    [[totalRandom]
                    , [v, c, c2]
                    , [vc, cv, cc]
                    , [cvc]
                    ]
            | a == 2 = 
                    [[totalRandom]
                    , [v, c, c2]
                    , [vc, cv, cc]
                    ]
            | a == 1 = 
                    [[totalRandom]
                    , [v, c, c2]
                    ]
            | a == 0 = 
                    [[totalRandom]
                    ]
        cvc = [1,2,3]
        vc = [2,3]
        cv = [1,2]
        cc = [1,3]
        v = [2]
        c = [1]
        c2 = [3]
        totalRandom = []
            
makeGuide :: [Int] -> [Int] -> [Int] -> [Int]
makeGuide _ guide [] = guide
makeGuide [] guide _ = guide
makeGuide nums [] pat = 
    makeGuide nums [0,0,0,0,0,0,0] pat
makeGuide nums guide pat = 
    makeGuide (tail nums) ((fst divided) ++ (head pat):[] ++ tail (snd divided)) (tail pat)
    where divided = (splitAt (nums!!0) guide)
  
assignPhoneme :: [Int] -> MorphosyntacticSystem -> PhonemeInventory -> RVar DeclensionPattern
assignPhoneme guide morph phonemes = do
    let VCSet phonemesV phonemesC = phonemes
    let MorphosyntacticSystem (GenderSystem a) (AnimacySystem b) (CaseSystem c) (NumberSystem d) (HonorificSystem e) (DefinitenessSystem f) (SpecificitySystem g) = morph
    h <- doGender (guide!!0) a phonemes
    i <- doAnimacy (guide!!1) b phonemes
    j <- doCase (guide!!2) c phonemes
    k <- doNumber (guide!!3) d phonemes
    l <- doHonorific (guide!!4) e phonemes
    m <- doDefiniteness (guide!!5) f phonemes
    n <- doSpecificity (guide!!6) g phonemes
    return (h,i,j,k,l,m,n) 

doGender :: Int -> [Gender] -> PhonemeInventory -> RVar (Int,[(Gender,Phoneme)])
doGender num genderSystem phonemes = do
    let VCSet phonemesV phonemesC = phonemes
    let pickedPhonemes 2 = sample (length genderSystem) phonemesV
        pickedPhonemes _ = sample (length genderSystem) phonemesC
    pnmes <- (pickedPhonemes num)
    return (num,zip genderSystem pnmes)

doAnimacy :: Int -> [Animacy] -> PhonemeInventory -> RVar (Int,[(Animacy,Phoneme)])
doAnimacy num animacySystem phonemes = do
    let VCSet phonemesV phonemesC = phonemes
    let pickedPhonemes 2 = sample (length animacySystem) phonemesV
        pickedPhonemes _ = sample (length animacySystem) phonemesC
    pnmes <- (pickedPhonemes num)
    return (num,zip animacySystem pnmes)

doCase :: Int -> [Case] -> PhonemeInventory -> RVar (Int,[(Case,Phoneme)])
doCase num caseSystem phonemes = do
    let VCSet phonemesV phonemesC = phonemes
    let pickedPhonemes 2 = sample (length caseSystem) phonemesV
        pickedPhonemes _ = sample (length caseSystem) phonemesC
    pnmes <- (pickedPhonemes num)
    return (num,zip caseSystem pnmes)
    
doNumber :: Int -> [Number] -> PhonemeInventory -> RVar (Int,[(Number,Phoneme)])
doNumber num numberSystem phonemes = do
    let VCSet phonemesV phonemesC = phonemes
    let pickedPhonemes 2 = sample (length numberSystem) phonemesV
        pickedPhonemes _ = sample (length numberSystem) phonemesC
    pnmes <- (pickedPhonemes num)
    return (num,zip numberSystem pnmes)
    
doHonorific :: Int -> [Honorific] -> PhonemeInventory -> RVar (Int,[(Honorific,Phoneme)])
doHonorific num honorificSystem phonemes = do
    let VCSet phonemesV phonemesC = phonemes
    let pickedPhonemes 2 = sample (length honorificSystem) phonemesV
        pickedPhonemes _ = sample (length honorificSystem) phonemesC
    pnmes <- (pickedPhonemes num)
    return (num,zip honorificSystem pnmes)

doDefiniteness :: Int -> [Definiteness] -> PhonemeInventory -> RVar (Int,[(Definiteness,Phoneme)])
doDefiniteness num definitenessSystem phonemes = do
    let VCSet phonemesV phonemesC = phonemes
    let pickedPhonemes 2 = sample (length definitenessSystem) phonemesV
        pickedPhonemes _ = sample (length definitenessSystem) phonemesC
    pnmes <- (pickedPhonemes num)
    return (num,zip definitenessSystem pnmes)
    
doSpecificity :: Int -> [Specificity] -> PhonemeInventory -> RVar (Int,[(Specificity,Phoneme)])
doSpecificity num specificitySystem phonemes = do
    let VCSet phonemesV phonemesC = phonemes
    let pickedPhonemes 2 = sample (length specificitySystem) phonemesV
        pickedPhonemes _ = sample (length specificitySystem) phonemesC
    pnmes <- (pickedPhonemes num)
    return (num,zip specificitySystem pnmes)

makeDeclension :: MorphosyntacticSystem -> PhonemeInventory -> RVar Declension
makeDeclension morph phonemes = do
    declPattern <- makeDeclensionPattern morph phonemes
    let combos = makeCombos morph
    decl <- sequence $ (map (makeMap phonemes declPattern)) combos
    return $ Declension decl

makeCombos :: MorphosyntacticSystem -> [Key]
makeCombos morph = (,,,,,,) <$> a <*> b <*> c <*> d <*> e <*> f <*> g
    where MorphosyntacticSystem (GenderSystem a) (AnimacySystem b) (CaseSystem c) (NumberSystem d) (HonorificSystem e) (DefinitenessSystem f) (SpecificitySystem g) = morph

makeMap :: PhonemeInventory -> DeclensionPattern -> Key -> RVar (Key, [Phoneme])
makeMap phonemes pattern key = do
    --key :: (Gender, Animacy, Case, Number, Honorific, Definiteness, Specifity)
    let (gen, ani, cas, num, hon, def, spe) = key
    let VCSet phonemesV phonemesC = phonemes
    let ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n)) = pattern
    c1 <- foobar1 key phonemesC ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n))
    v <- foobar2 key phonemesV ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n))
    c2 <- foobar3 key phonemesC ((a,b),(c,d),(e,f),(g,h),(i,j),(k,l),(m,n))
    return (key, [c1,v,c2])

foobar1 :: Key -> [Phoneme] -> DeclensionPattern -> RVar Phoneme
foobar1 key _ ((1,b),(_,_),(_,_),(_,_),(_,_),(_,_),(_,_)) = do
    let (gen, _, _, _, _, _, _) = key
    return $ snd $ head $ filter (\a -> fst a == gen) b
foobar1 key _ ((_,_),(1,b),(_,_),(_,_),(_,_),(_,_),(_,_)) = do
    let (_, ani, _, _, _, _, _) = key
    return $ snd $ head $ filter (\a -> fst a == ani) b
foobar1 key _ ((_,_),(_,_),(1,b),(_,_),(_,_),(_,_),(_,_)) = do
    let (_, _, cas, _, _, _, _) = key
    return $ snd $ head $ filter (\a -> fst a == cas) b
foobar1 key _ ((_,_),(_,_),(_,_),(1,b),(_,_),(_,_),(_,_)) = do
    let (_, _, _, num, _, _, _) = key
    return $ snd $ head $ filter (\a -> fst a == num) b
foobar1 key _ ((_,_),(_,_),(_,_),(_,_),(1,b),(_,_),(_,_)) = do
    let (_, _, _, _, hon, _, _) = key
    return $ snd $ head $ filter (\a -> fst a == hon) b
foobar1 key _ ((_,_),(_,_),(_,_),(_,_),(_,_),(1,b),(_,_)) = do
    let (_, _, _, _, _, def, _) = key
    return $ snd $ head $ filter (\a -> fst a == def) b
foobar1 key _ ((_,_),(_,_),(_,_),(_,_),(_,_),(_,_),(1,b)) = do
    let (_, _, _, _, _, _, spe) = key
    return $ snd $ head $ filter (\a -> fst a == spe) b
foobar1 key phonemes ((_,_),(_,_),(_,_),(_,_),(_,_),(_,_),(_,_)) = do
    foobar <- choice phonemes
    return $ foobar
    
foobar2 :: Key -> [Phoneme] -> DeclensionPattern -> RVar Phoneme
foobar2 key _ ((2,b),(_,_),(_,_),(_,_),(_,_),(_,_),(_,_)) = do
    let (gen, _, _, _, _, _, _) = key
    return $ snd $ head $ filter (\a -> fst a == gen) b
foobar2 key _ ((_,_),(2,b),(_,_),(_,_),(_,_),(_,_),(_,_)) = do
    let (_, ani, _, _, _, _, _) = key
    return $ snd $ head $ filter (\a -> fst a == ani) b
foobar2 key _ ((_,_),(_,_),(2,b),(_,_),(_,_),(_,_),(_,_)) = do
    let (_, _, cas, _, _, _, _) = key
    return $ snd $ head $ filter (\a -> fst a == cas) b
foobar2 key _ ((_,_),(_,_),(_,_),(2,b),(_,_),(_,_),(_,_)) = do
    let (_, _, _, num, _, _, _) = key
    return $ snd $ head $ filter (\a -> fst a == num) b
foobar2 key _ ((_,_),(_,_),(_,_),(_,_),(2,b),(_,_),(_,_)) = do
    let (_, _, _, _, hon, _, _) = key
    return $ snd $ head $ filter (\a -> fst a == hon) b
foobar2 key _ ((_,_),(_,_),(_,_),(_,_),(_,_),(2,b),(_,_)) = do
    let (_, _, _, _, _, def, _) = key
    return $ snd $ head $ filter (\a -> fst a == def) b
foobar2 key _ ((_,_),(_,_),(_,_),(_,_),(_,_),(_,_),(2,b)) = do
    let (_, _, _, _, _, _, spe) = key
    return $ snd $ head $ filter (\a -> fst a == spe) b
foobar2 key phonemes ((_,_),(_,_),(_,_),(_,_),(_,_),(_,_),(_,_)) = do
    foobar <- choice phonemes
    return $ foobar
    
foobar3 :: Key -> [Phoneme] -> DeclensionPattern -> RVar Phoneme
foobar3 key _ ((3,b),(_,_),(_,_),(_,_),(_,_),(_,_),(_,_)) = do
    let (gen, _, _, _, _, _, _) = key
    return $ snd $ head $ filter (\a -> fst a == gen) b
foobar3 key _ ((_,_),(3,b),(_,_),(_,_),(_,_),(_,_),(_,_)) = do
    let (_, ani, _, _, _, _, _) = key
    return $ snd $ head $ filter (\a -> fst a == ani) b
foobar3 key _ ((_,_),(_,_),(3,b),(_,_),(_,_),(_,_),(_,_)) = do
    let (_, _, cas, _, _, _, _) = key
    return $ snd $ head $ filter (\a -> fst a == cas) b
foobar3 key _ ((_,_),(_,_),(_,_),(3,b),(_,_),(_,_),(_,_)) = do
    let (_, _, _, num, _, _, _) = key
    return $ snd $ head $ filter (\a -> fst a == num) b
foobar3 key _ ((_,_),(_,_),(_,_),(_,_),(3,b),(_,_),(_,_)) = do
    let (_, _, _, _, hon, _, _) = key
    return $ snd $ head $ filter (\a -> fst a == hon) b
foobar3 key _ ((_,_),(_,_),(_,_),(_,_),(_,_),(3,b),(_,_)) = do
    let (_, _, _, _, _, def, _) = key
    return $ snd $ head $ filter (\a -> fst a == def) b
foobar3 key _ ((_,_),(_,_),(_,_),(_,_),(_,_),(_,_),(3,b)) = do
    let (_, _, _, _, _, _, spe) = key
    return $ snd $ head $ filter (\a -> fst a == spe) b
foobar3 key phonemes ((_,_),(_,_),(_,_),(_,_),(_,_),(_,_),(_,_)) = do
    foobar <- choice phonemes
    return $ foobar