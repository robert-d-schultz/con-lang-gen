module Test
( superSplit
, superSplit2
) where

import Prelude
import Data.List

superSplit :: String -> [[(String, String)]]
superSplit [] = []
superSplit xs = nub bar where
  foo = map (`splitAt` xs) [0..(length xs)] -- [(RTB,[]), (RT,B), (R,TB), ([],RTB)]
  bar = concatMap (\(a,b) -> if null a ||  null b then [[(a,b)]] else (++) <$> superSplit a <*> superSplit b) foo

superSplit2 :: String -> [[(String, String)]]
superSplit2 [] = []
superSplit2 xs = bar where
  foo = map (`splitAt` xs) [0..(length xs)] -- [(RTB,[]), (RT,B), (R,TB), ([],RTB)]
  bar = concatMap (\(a,b) -> if null a ||  null b then [[(a,b)]] else (++) <$> superSplit a <*> superSplit b) foo
