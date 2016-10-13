{-# LANGUAGE ScopedTypeVariables #-}
module PhonemeGen
( randomValue
, generateConsonant
, generateVowel
) where

-- Import
import           Data.Random        hiding (sample)
import           Data.Random.Extras hiding (shuffle)
import           Data.RVar
import           Prelude

-- Data structures
import           PhonemeType


-- Makes a random phoneme inventory (doesn't check for duplicates)
makeRandomInventory :: Int -> Int -> RVar PhonemeInventory
makeRandomInventory numc numv = PhonemeInventory <$> makeConsonantList numc (return []) <*> makeVowelList numv (return [])

makeConsonantList:: Int -> RVar [Consonant] -> RVar [Consonant]
makeConsonantList 0 cons = cons
makeConsonantList num cons = makeConsonantList (num-1) (addToInventory generateConsonant cons)

makeVowelList:: Int -> RVar [Vowel] -> RVar [Vowel]
makeVowelList 0 vows = vows
makeVowelList num vows = makeVowelList (num-1) (addToInventory generateVowel vows)

addToInventory :: RVar a -> RVar [a] -> RVar [a]
addToInventory new old = (:) <$> new <*> old

-- Picks a random value of an Enum data type
randomValue :: forall t. (Bounded t, Enum t) => RVar t
randomValue = toEnum <$> uniform (fromEnum(minBound :: t)) (fromEnum(maxBound :: t))

-- Generates a completely random consonant
generateConsonant :: RVar Consonant
generateConsonant = do
    place<- randomValue :: RVar Place
    manner <- randomValue :: RVar Manner
    voice <- randomValue :: RVar Voice
    return $ Consonant place manner voice "fooc"

-- Generates a completely random vowel
generateVowel :: RVar Vowel
generateVowel = do
    height <- randomValue :: RVar Height
    backness <- randomValue :: RVar Backness
    roundedness <- randomValue :: RVar Roundedness
    return $ Vowel height backness roundedness "foov"
