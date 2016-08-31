module PhonologyGen
(makeConsonantPhonemeInventory
) where

-- Import
import Prelude
import Data.RVar
import ConsonantPhoneGen

-- Data structures
import PhonemeType2

-- Load from files

-- Make phoneme inventory (doesn't check for duplicates yet)
makeConsonantPhonemeInventory :: Int -> RVar ConsonantPhonemeInventory
makeConsonantPhonemeInventory num = fmap ConsonantPhonemeInventory (makeConsonantList num (return []))

makeConsonantList:: Int -> RVar [Consonant] -> RVar [Consonant]
makeConsonantList 0 cons = cons
makeConsonantList num cons = makeConsonantList (num-1) (addToInventory generateConsonant cons)

addToInventory :: RVar Consonant -> RVar [Consonant] -> RVar [Consonant]
addToInventory newCon oldCons = (:) <$> newCon <*> oldCons
