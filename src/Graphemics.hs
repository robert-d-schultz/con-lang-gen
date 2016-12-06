module Graphemics
(
) where

-- outputs: unicode numbers for the unused blocks -> use these for the outputs, worry about the font later

-- generate an alphabet based on phonemes
-- monographs/digraphs allowed
-- includes abjads and abugidas
generateAlphabet :: [Phoneme] -> [(Phoneme, [Int])]
generateAlphabet = out where


-- generate a syllabary from... a list of (used) syllables?
-- go through the full lexicon (which would include all inflected and uninflected words) and make a list of syllables
generateSyllabary :: [Syllable] -> [(Syllable, Int)]
generateSyllabary = out where


-- generate logographs, one for each lexicon entry, inflection entry
-- maybe two/three/four for each lexicon entry? related too much to morphology and therefore semantics
generateLogography :: [Morpheme] -> [(Morpheme, [Int])]
generateLogography = where
