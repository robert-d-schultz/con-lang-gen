module Gen.WritingSystem
( generateAlphabet
, generateSyllabary
, generateLogography
) where

import Gen.Phoneme

-- generate an alphabet based on phonemes
-- monographs/digraphs allowed
-- includes abjads and abugidas
generateAlphabet :: [Phoneme] -> [(Phoneme, [Int])]
generateAlphabet phonemes = zip phonemes (map (:[]) [983040..])


-- generate a syllabary from... a list of (used) syllables?
-- go through the full lexicon (which would include all inflected and uninflected words) and make a list of syllables
generateSyllabary :: [Syllable] -> [(Syllable, Int)]
generateSyllabary sylls = zip phones (map (:[]) [983040..])


-- generate logographs, one for each lexicon entry, inflection entry
-- maybe two/three/four for each lexicon entry? related too much to morphology and therefore semantics
generateLogography :: [Morpheme] -> [(Morpheme, [Int])]
generateLogography morphs = zip phones (map (:[]) [983040..])
