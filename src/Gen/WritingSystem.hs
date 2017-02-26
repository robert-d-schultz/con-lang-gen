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

import Gen.Phonology

--pick writing systems
generateWritingSystem :: [Phoneme] -> [Syllable] -> [((String,LexCat), Morpheme)] -> RVar ([(Phoneme, Int)], [(Syllable, Int)], [(((String, LexCat), Morpheme), Int)])
generateWritingSystem phonemes [] morphs = choice [ (generateAlphabet phonemes, [], [])
                                                  , ([], [], generateLogography morphs 983040)
                                                  ]
generateWritingSystem phonemes sylls morphs = choice [ (generateAlphabet phonemes, [], [])
                                                     , ([], generateSyllabary sylls 983040, [])
                                                     , ([], [], generateLogography morphs 983040)
                                                     ]

-- generate an alphabet based on phonemes
-- monographs/digraphs allowed (not yet)
-- includes abjads and abugidas (not yet)
generateAlphabet :: [Phoneme] -> [(Phoneme, Int)]
generateAlphabet phonemes = zip phonemes [983040..]


-- generates a "true" syllabary
-- need "false" syllabary later
generateSyllabary :: [Syllable] -> Int -> [(Syllable, Int)]
generateSyllabary sylls n = zip sylls [n..]


-- make all syllables
makeAllSyllables :: [[Phoneme]] -> [Phoneme] -> [[Phoneme]] -> [Syllable]
makeAllSyllables onsets nucleuss codas = Syllable <$> onsets <*> nucleuss <*> codas


-- generate logographs, one for each lexicon entry, inflection entry
-- maybe two/three/four for each lexicon entry? related too much to morphology and therefore semantics
generateLogography :: [((String, LexCat), Morpheme)] -> Int -> [(((String, LexCat), Morpheme), Int)]
generateLogography morphs n = zip morphs [n..]
