module WordStructureGen where

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

data AvailableStructure = AvailableStructure [SyllableStructure]
data SyllableStructure = CVC | VC | CV | V
    deriving (Show, Eq, Ord, Enum, Bounded)
data Regex
    = RConcat [Regex]
    | RPartition [Regex] -- like RConcat but interacts with RRepeatMin
    | RAlternate [Regex]
    | RSylls [SyllableStructure]
    | RRepeatMin Int Regex

a -+- b = RConcat [a, b]
infixl 5 -+-

anySyllableStructure :: Regex
anySyllableStructure = RAlternate (fmap (RSylls . pure) [CVC, VC, CV, V])

unstructured :: Regex
unstructured = RRepeatMin 0 anySyllableStructure

-- Program
makeWordStructure :: RVar Regex
makeWordStructure =
    (AvailableStructure <$> choice (subsequences [CVC ..])) >>= makeWordStructure'

makeWordStructure' :: AvailableStructure -> RVar Regex
makeWordStructure' (AvailableStructure avail) = do
    let possibilities =
            [ totalUnstructured
            , pickBeginning
            , pickMiddle
            , pickEnd
            , structuredParts 1
            , structuredParts 2
            , structuredParts 3
            ]
    join (choice possibilities)
    where
    totalUnstructured = return $ unstructured
    unstructuredMiddle = do
        beginning <- makeWordSubStruct 2
        end <- makeWordSubStruct 2
        return $ RPartition [beginning, unstructured, end]
    pickBeginning = do
        beginning <- makeWordSubStruct 2
        return $ RPartition [beginning, unstructured]
    pickMiddle = do
        middle <- makeWordSubStruct 2
        return $ RPartition [unstructured, middle, unstructured]
    pickEnd = do
        end <- makeWordSubStruct 2
        return $ RPartition [unstructured, end]
    structuredParts numParts = do
        parts <- replicateM numParts (makeWordSubStruct 2)
        return $ RPartition parts
    makeWordSubStruct len = do
        sylls <- replicateM len (choice avail)
        return $ RRepeatMin 1 (RSylls sylls)

makeWord :: Regex -> RVar String
makeWord (RConcat rs) = concat <$> mapM makeWord rs
makeWord (RPartition rs) = makeWord (RConcat rs)
makeWord (RAlternate rs) = choice rs >>= makeWord
makeWord (RSylls rs) = return $ concat (fmap show rs)
makeWord (RRepeatMin min r) = do
    n <- uniform min (min + 5)
    concat <$> replicateM n (makeWord r)
