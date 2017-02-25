module Gen.WritingSystem
( generateWritingSystem
, generateAlphabet
, generateSyllabary
, makeAllSyllables
, generateLogography
) where

import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)

import Data.Phoneme
import Data.Other
import Data.Inflection

import Gen.Phoneme

--pick writing systems
generateWritingSystem :: [Phoneme] -> [Syllable] -> [((String,LexCat), Morpheme)] -> RVar ([(Phoneme, Int)], [(Syllable, Int)], [(((String, LexCat), Morpheme), Int)])
generateWritingSystem phonemes sylls morphs = choice [ (generateAlphabet phonemes, [], [])
                                                 , ([], generateSyllabary sylls 983040, [])
                                                 , ([], [], generateLogography morphs 983040)
                                                 ]

-- generate an alphabet based on phonemes
-- monographs/digraphs allowed (not yet)
-- includes abjads and abugidas (not yet)
generateAlphabet :: [Phoneme] -> [(Phoneme, Int)]
generateAlphabet phonemes = zip phonemes [983040..]


-- generate a syllabary from... a list of (used) syllables?
-- go through the full lexicon (which would include all inflected and uninflected words) and make a list of syllables
-- actually let's see how it does when we know the valid onsets, vowels, and codas
generateSyllabary :: [Syllable] -> Int -> [(Syllable, Int)]
generateSyllabary sylls n = zip sylls [n..]

-- Make all syllables
makeAllSyllables :: [[Phoneme]] -> [Phoneme] -> [[Phoneme]] -> [Syllable]
makeAllSyllables onsets nucleuss codas = Syllable <$> onsets <*> nucleuss <*> codas


-- generate logographs, one for each lexicon entry, inflection entry
-- maybe two/three/four for each lexicon entry? related too much to morphology and therefore semantics
generateLogography :: [((String, LexCat), Morpheme)] -> Int -> [(((String, LexCat), Morpheme), Int)]
generateLogography morphs n = zip morphs [n..]
