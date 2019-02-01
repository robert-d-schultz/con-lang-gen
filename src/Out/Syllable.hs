module Out.Syllable
( syllabifyWord
) where

import ClassyPrelude hiding (Word, maximumBy)

import Data.List (findIndices, findIndex)

import HelperFunctions

import Data.Phoneme
import Data.Word
import Data.Inflection
import Data.Language

-- Word's can be made up of both MorphemeS and MorphemeP's
-- The syllabifyWord function needs to deal with the MorphemeP's
-- They need to be added to previous/next syllable if possible
-- Otherwise a central/mid vowel needs to be inserted to save it

-- Syllabification
-- Given a word and sonority hierarchy, syllabify the word
syllabifyWord :: Language -> Word -> Maybe [Syllable]
syllabifyWord lang (Word m (ConsonantalRoot _ Root rads) (PatternMorph _ Transfix patts)) = syllabifyConsRoot lang rads patts
syllabifyWord _ (Word _ ConsonantalRoot{} _) = Nothing
syllabifyWord _ (Word _ _ ConsonantalRoot{}) = Nothing
syllabifyWord _ (Word _ PatternMorph{} _) = Nothing
syllabifyWord _ (Word _ _ PatternMorph{}) = Nothing
syllabifyWord lang (Word _ leftM rightM) = syllabifyWord lang leftM ++ syllabifyWord lang rightM
syllabifyWord _ (MorphemeS _ _ sylls) = Just sylls
syllabifyWord lang (MorphemeP _ _ ps) = sylls where
  groups = breakPhonemes lang ps []
  sylls = map (makeSyllable lang) <$> groups
syllabifyWord _ ConsonantalRoot{} = Nothing
syllabifyWord _ PatternMorph{} = Nothing

-- Given a group of phonemes (and a tone?), make a proper syllable structure
makeSyllable :: Language -> [Phoneme] -> Syllable
makeSyllable lang ps = out where
  nuclei = getNuclei lang
  out = maybe (Syllable ps Blank [Blank] NONET NONES) foo (findIndex (`elem` nuclei) ps)
  foo i = Syllable onset nucleus coda NONET NONES where
    (onset, nucleus:coda) = splitAt i ps

-- Interweave Consonantal (Semitic) roots and vowel Patterns
-- Doesn't check if it results in proper syllables
syllabifyConsRoot :: Language -> [ConsCluster] -> [Syllable] -> Maybe [Syllable]
syllabifyConsRoot _ [] patts = Just (filter (Syllable [] Blank [] NONET NONES /=) patts)
syllabifyConsRoot lang rads [] = Nothing
syllabifyConsRoot lang (r1:r2:rads) (p1:p2:patts)
    | p2 == Syllable [] Blank [] NONET NONES = (:) p1 <$> syllabifyConsRoot lang ((r1 ++ r2):rads) patts
    | isNothing (syllabifyConsRoot lang (r2:rads) patts) = (:) p1 <$> Just [p2{getOnset = r1, getCoda = concat (r2:rads)}]
    | otherwise = (:) p1 <$> syllabifyConsRoot lang (r2:rads) (p2{getOnset = r1}:patts)
syllabifyConsRoot lang [r] (p1:p2:patts)
    | p2 == Syllable [] Blank [] NONET NONES = (:) p1 <$> syllabifyConsRoot lang [r] patts
    | otherwise = (:) p1 <$> syllabifyConsRoot lang [] (p2{getOnset = r}:patts)
syllabifyConsRoot lang (r:rads) [p]
    | p == Syllable [] Blank [] NONET NONES = Nothing
    | otherwise = Just [p{getCoda = concat (r:rads)}]

{- This one is based on Sonority Hierarchy, and not valid CC/Nuclei...
breakPhonemes :: [Phoneme] -> [Phoneme] -> [[Phoneme]] -> [[Phoneme]]
breakPhonemes [] syll sonhier = [syll]
breakPhonemes phonemes [] sonhier = breakPhonemes (initMay phonemes) [lastMay phonemes] sonhier
breakPhonemes phonemes syll sonhier
  -- start a new syllable for a vowel immediately after another vowel
  | retrieveSon sonhier (lastMay phonemes) == length sonhier + 1
    && retrieveSon sonhier (headMay syll) == length sonhier + 1                 = breakPhonemes phonemes [] sonhier ++ [syll]
  -- start new syllable when at local minimum (edge case)
  | length syll < 2 &&
    retrieveSon sonhier (lastMay phonemes) > retrieveSon sonhier (headMay syll) = breakPhonemes (initMay phonemes) (lastMay phonemes : syll) sonhier
  -- start new syllable when at local minimum
  | length syll >= 2 &&
    retrieveSon sonhier (lastMay phonemes) > retrieveSon sonhier (headMay syll)
    && retrieveSon sonhier (syll !! 1) >= retrieveSon sonhier (headMay syll)    = breakPhonemes phonemes [] sonhier ++ [syll]
  -- otherwise add next phoneme to syllable
  | otherwise                                                                   = breakPhonemes (initMay phonemes) (lastMay phonemes : syll) sonhier
-}

-- Input the raw string of phonemes, output groups of phonemes that correspond to syllables
breakPhonemes :: Language -> [Phoneme] -> [Phoneme] -> Maybe [[Phoneme]]
breakPhonemes _ [] _ = Just []
breakPhonemes lang ps [] = do
  let nuclei = getNuclei lang
  let codas = getCodaCCs lang
  i <- lastMay (findIndices (`elem` nuclei) ps)
  let (r, n:c) = splitAt i ps
  if c `elem` codas then breakPhonemes lang r (n:c) else Nothing
breakPhonemes lang ps (n:c) = do
  let onsets = getOnsetCCs lang
  --let nuclei = getNuclei lang
  --let codas = getCodaCCs lang
  (r, o) <- dropUntil 0 (`elem` onsets) ps
  f <- breakPhonemes lang r []
  return (f ++ [o ++ n:c])

-- dropUntil and takeRUntilNot do different but similar things
-- Imagine a language where /br/ was valid but /r/ was not
-- dropUntil would allow /br/ in intermediate onsets, takeRUntilNot wouldn't
dropUntil :: Int -> ([a] -> Bool) -> [a] -> Maybe ([a], [a])
dropUntil i b xs
  | i == length xs = Nothing
  | (b . snd) (splitAt i xs) = Just (splitAt i xs)
  | otherwise = dropUntil (i+1) b xs

{-
takeRUntilNot :: Int -> [a] -> ([a] -> Bool) -> Maybe [a]
takeRUntilNot 0 _ _ = Nothing
takeRUntilNot _ [] _ = Nothing
takeRUntilNot n xs b
  | n < length xs = Nothing
  | not.b $ takeR n xs = Just $ takeR (n-1) xs
  | otherwise = takeRUntilNot (n+1) xs b

takeR :: Int -> [a] -> [a]
takeR n l = go (drop n l) l
  where
    go [] r = r
    go (_:xs) (_:ys) = go xs ys
-}
