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

-- Import components
import PhonologyGen
import GrammarGen
--import WordStructureGen

{-
main :: IO ()
main = do
    let foo = makeWordStructure >>= makeWord
    bar <- Sampler.sample foo
    print bar
-}

main :: IO ()
main = do
    inputD <- loadInputData
    phonemeInventory <- Sampler.sample $ makePhonemeInventory inputD 4 10 4 10
    inputD2 <- loadInputData2
    sys <- Sampler.sample $ pickSystem inputD2
    declension <- Sampler.sample $ makeDeclension sys phonemeInventory
    --wordStructure <- Sampler.sample (makeWordStructure phonemeInventory)
    print declension

