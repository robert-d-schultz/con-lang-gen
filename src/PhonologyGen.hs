module PhonologyGen
(makePhonemeInventory
) where

-- Import libraries
import Prelude hiding (Word)
--import System.Random
--import Control.Monad
--import Data.Random hiding (sample)
--import qualified Data.Random.Sample as Sampler
--import Data.Random.Extras hiding (shuffle)
--import Data.Functor
--import Data.List
--import Control.Applicative
--import System.IO
import Data.RVar

-- Data structures
import PhonemeType
import ConsonantPhoneGen

-- Load from files


-- Make phoneme inventory (doesn't check for duplicates yet)
makePhonemeInventory :: Num -> PhonemeInventory -> RVar PhonemeInventory
makePhonemeInventory numConsonants [] = [generateConsonant]
makePhonemeInventory numConsonants inventory = makePhonemeInventory (numConsonants-1) (generateConsonant:inventory)
makePhonemeInventory 0 inventory = inventory
