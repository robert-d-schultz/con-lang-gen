-- Word generator
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


--Read meanings
readMeanings :: FilePath -> IO [Relationship]
readMeanings fileName = do
    meanings_text <- readFile fileName
    let meanings_string = read meanings_text :: [[[Char]]]
    let heads = map head meanings_string
    let tails = map tail meanings_string
    let heads_meaning = map Meaning heads
    let tails_meaning = map (map Meaning) tails
    return $ zipWith Relationship heads_meaning tails_meaning

-- Make syllable
makeSyllable :: PhonemeInventory -> RVar Syllable
makeSyllable (VCSet v c) = do
    onset <- Sampler.sample (take 1 <$> shuffle c)
    nucleus <- Sampler.sample (take 1 <$> shuffle v)
    coda <- Sampler.sample (take 1 <$> shuffle c)
    let syll = Syllable (Onset onset) (Nucleus (head nucleus)) (Coda coda)
    return syll
    
-- Make word
makeWord :: PhonemeInventory -> RVar Word
makeWord phonemes = do
    syllList <- replicateM 3 (makeSyllable phonemes)
    return $ Word syllList