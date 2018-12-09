{-# LANGUAGE NoImplicitPrelude #-}
module Data.Soundchange
( PhonemeR(..)
, Rule(..)
) where

import ClassyPrelude

import Data.Phoneme

data PhonemeR = ConsonantR
           { cplaceR :: Maybe Place
           , cmannerR :: Maybe Manner
           , cvoiceR :: Maybe Phonation
           , cairstreamR :: Maybe Airstream
           }
           | VowelR
           { vheightR :: Maybe Height
           , vbacknessR :: Maybe Backness
           , vroundednessR :: Maybe Roundedness
           , vlengthR :: Maybe Length
           }
           | DiphthongR
           { dheight1R :: Maybe Height
           , dbackness1R :: Maybe Backness
           , droundedness1R :: Maybe Roundedness
           , dheight2R :: Maybe Height
           , dbackness2R :: Maybe Backness
           , droundedness2R :: Maybe Roundedness
           , dlengthR :: Maybe Length
           }
           | SyllableBoundary
           | WordBoundary deriving (Eq, Ord)


instance Show PhonemeR where
  show (ConsonantR p m v a) = "[+Consonant]"
                            ++ fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> p)
                            ++ fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> m)
                            ++ fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> v)
                            ++ fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> a)
  show (VowelR h b r l) = "[+Vowel]"
                            ++ fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> h)
                            ++ fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> b)
                            ++ fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> r)
                            ++ fromMaybe "" ((\x -> "[+" ++ show x ++ "]") <$> l)
  show DiphthongR{} = "blagh"
  show SyllableBoundary = "σ"
  show WordBoundary = "#"

data Rule = Rule PhonemeR PhonemeR (Maybe PhonemeR) (Maybe PhonemeR) | NoChange

-- A > B / X__Y
instance Show Rule where
  show (Rule a b Nothing Nothing)   = show a ++ " > " ++ show b ++ " / [+Any]__[+Any]"
  show (Rule a b Nothing (Just f))  = show a ++ " > " ++ show b ++ " / [+Any]__" ++ show f
  show (Rule a b (Just p) Nothing)  = show a ++ " > " ++ show b ++ " / " ++ show p ++ "__[+Any]"
  show (Rule a b (Just p) (Just f)) = show a ++ " > " ++ show b ++ " / " ++ show p ++ "__" ++ show f
  show NoChange = "No change"
