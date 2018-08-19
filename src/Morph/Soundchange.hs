{-# LANGUAGE NoImplicitPrelude #-}
module Morph.Soundchange
( conditionedSplit
) where

import ClassyPrelude

import Data.RVar
import Data.Random.Extras
import Control.Monad
import Data.Maybe
import Data.List (nub)

import Data.Phoneme
import Data.Inflection
import Data.Other

import Gen.Phonology
import Gen.Phonotactics

import Constants

import Debug.Trace

-- conditioned split adds a new contrast
-- conditioned merge doesn't and "splits" the phoneme between itself and a different one

data PhonemeR = ConsonantR
           { cplaceR :: Maybe Place
           , cmannerR :: Maybe Manner
           , cvoiceR :: Maybe Phonation
           }
           | VowelR
           { vheightR :: Maybe Height
           , vbacknessR :: Maybe Backness
           , vroundednessR :: Maybe Roundedness
           , vlengthR :: Maybe Length
           , vtoneR :: Maybe Tone
           }
           | DiphthongR
           { dheight1R :: Maybe Height
           , dbackness1R :: Maybe Backness
           , droundedness1R :: Maybe Roundedness
           , dheight2R :: Maybe Height
           , dbackness2R :: Maybe Backness
           , droundedness2R :: Maybe Roundedness
           , dlengthR :: Maybe Length
           , dtoneR :: Maybe Tone
           }
           | BlankR deriving (Eq, Ord, Read)

instance Show PhonemeR where
  show (ConsonantR p m v) = fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> p)
                         ++ fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> m)
                         ++ fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> v)
  show (VowelR h b r l t) = fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> h)
                         ++ fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> b)
                         ++ fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> r)
                         ++ fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> l)
                         ++ fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> t)
  show DiphthongR{} = "blagh"
  show BlankR = "should this even exist"

data Rule = Rule PhonemeR PhonemeR (Maybe PhonemeR) (Maybe PhonemeR)

-- A > B / X__Y
instance Show Rule where
  show (Rule a b Nothing Nothing)   = show a ++ " > " ++ show b ++ " / #__#"
  show (Rule a b Nothing (Just f))  = show a ++ " > " ++ show b ++ " / #__" ++ show f
  show (Rule a b (Just p) Nothing)  = show a ++ " > " ++ show b ++ " / " ++ show p ++ "__#"
  show (Rule a b (Just p) (Just f)) = show a ++ " > " ++ show b ++ " / " ++ show p ++ "__" ++ show f

-- generate phonological rule
-- start with batshit insane ones, I guess
-- just pick random values
generateRule :: Language -> RVar Rule
generateRule lang = do
  -- phoneme A changes into phoneme B...
  (soundA, soundB) <- join $ choice [createCPatterns lang, createVPatterns lang]

  -- ...preceding phoneme P...
  soundP <- join $ choice [ return Nothing
                          , Just <$> createCPattern lang
                          , Just <$> createVPattern lang
                          ]
  -- ...and following phoneme F.
  soundF <- join $ choice [ return Nothing
                          , Just <$> createCPattern lang
                          , Just <$> createVPattern lang
                          ]
  return $ Rule soundA soundB soundP soundF

createCPattern lang = do
  let (places, manners, phonations) = getCMap lang
  pr <- choice (Nothing : map Just places)
  mr <- choice (Nothing : map Just manners)
  hr <- choice (Nothing : map Just phonations)
  return (ConsonantR pr mr hr)

createCPatterns lang = do
  let (places, manners, phonations) = getCMap lang
  pra <- choice (Nothing : map Just places)
  mra <- choice (Nothing : map Just manners)
  hra <- choice (Nothing : map Just phonations)

  prb <- choice (Nothing : delete pra (map Just places))
  mrb <- choice (Nothing : delete mra (map Just manners))
  hrb <- choice (Nothing : delete hra (map Just phonations))
  return (ConsonantR pra mra hra, ConsonantR prb mrb hrb)

createVPattern lang = do
  let (heights, backs, rounds, lengths, tones) = getVMap lang
  hr <- choice (Nothing : map Just heights)
  br <- choice (Nothing : map Just backs)
  rr <- choice (Nothing : map Just rounds)
  lr <- choice (Nothing : map Just lengths)
  tr <- choice (Nothing : map Just tones)
  return (VowelR hr br rr lr tr)

createVPatterns lang = do
  let (heights, backs, rounds, lengths, tones) = getVMap lang
  hra <- choice (Nothing : map Just heights)
  bra <- choice (Nothing : map Just backs)
  rra <- choice (Nothing : map Just rounds)
  lra <- choice (Nothing : map Just lengths)
  tra <- choice (Nothing : map Just tones)

  hrb <- choice (Nothing : delete hra (map Just heights))
  brb <- choice (Nothing : delete bra (map Just backs))
  rrb <- choice (Nothing : delete rra (map Just rounds))
  lrb <- choice (Nothing : delete lra (map Just lengths))
  trb <- choice (Nothing : delete tra (map Just tones))
  return (VowelR hra bra rra lra tra, VowelR hrb brb rrb lrb trb)

-- applies a phonological rule to a word
-- goes through each phoneme and sees if it fits the conditions
-- if so, it changes that phoneme as specified
executeRule :: Rule -> [Phoneme] -> [Phoneme] -> [Phoneme]
executeRule _ ys [] = ys
executeRule (Rule a b p f) ys (x:xs)
    | and [ comparePhonemeR p (lastMay ys)
          , comparePhonemeR (Just a) (Just x)
          , comparePhonemeR f (headMay xs)
          , not $ impConsonants (applyRule b x) -- it shouldn't execute the rule if the resultant new phoneme is an impossible one.....
          ] = executeRule (Rule a b p f) (ys++[applyRule b x]) xs
    | otherwise = executeRule (Rule a b p f) (ys++[x]) xs


applyRule :: PhonemeR -> Phoneme -> Phoneme
applyRule (ConsonantR pr mr hr) (Consonant p m h) = Consonant (fromMaybe p pr) (fromMaybe m mr) (fromMaybe h hr)
applyRule (VowelR hr br rr lr tr) (Vowel h b r l t) = Vowel (fromMaybe h hr) (fromMaybe b br) (fromMaybe r rr) (fromMaybe l lr) (fromMaybe t tr)

comparePhonemeR :: Maybe PhonemeR -> Maybe Phoneme -> Bool
comparePhonemeR (Just (ConsonantR pr mr hr)) (Just (Consonant p m h)) = and [ fromMaybe True ((p ==) <$> pr)
                                                                            , fromMaybe True ((m ==) <$> mr)
                                                                            , fromMaybe True ((h ==) <$> hr)
                                                                            ]
comparePhonemeR (Just (VowelR hr br rr lr tr)) (Just (Vowel h b r l t)) = and [ fromMaybe True ((h ==) <$> hr)
                                                                              , fromMaybe True ((b ==) <$> br)
                                                                              , fromMaybe True ((r ==) <$> rr)
                                                                              , fromMaybe True ((l ==) <$> lr)
                                                                              , fromMaybe True ((t ==) <$> tr)
                                                                              ]
comparePhonemeR Nothing Nothing = True
comparePhonemeR _ _ = False




-- given a phonological rule, change around the Lexicon and Declension
conditionedSplit :: Language -> RVar Language
conditionedSplit lang = do
  rule <- generateRule lang

  -- test rule
  -- Anything > [+Stop] / #_[+Vowel]
{-
  let pa = ConsonantR Nothing Nothing Nothing
  let pb = ConsonantR Nothing (Just STOP) Nothing
  let pf = VowelR Nothing Nothing Nothing Nothing Nothing
  let rule = Rule pa pb Nothing (Just pf)
-}

  let roots = getRoots lang
  let manSyss = getManSyss lang
  let rootsN = map (\(x, Morpheme y) -> (x, Morpheme (executeRule rule [] y))) roots
  let manSyssN = map (\(ManifestSystem x y z) -> ManifestSystem x y (map (\(Morpheme w, v) -> (Morpheme (executeRule rule [] w), v)) z)) manSyss

  -- take inventory of consonants used in the entire lexicon
  let lexPhonemes = nub (concatMap (\(_, Morpheme y) -> y) rootsN)
  let declPhonemes = concatMap (\(ManifestSystem _ _ z) -> (concatMap (\(Morpheme w, _) -> w) z)) manSyssN
  let allPhonemes = nub $ lexPhonemes ++ declPhonemes
  let cInvN = filter isConsonant allPhonemes
  let vInvN = filter isVowel allPhonemes

  return $ trace (show rule) lang{getCInv = cInvN, getVInv = vInvN, getRoots = rootsN, getManSyss = manSyssN}

isVowel :: Phoneme -> Bool
isVowel Vowel{} = True
isVowel _ = False

isConsonant :: Phoneme -> Bool
isConsonant Consonant{} = True
isConsonant _ = False

{-
conditionedSplitManner ::
conditionedSplitManner =


conditionedMergeManner ::
conditionedMergeManner =
-}
