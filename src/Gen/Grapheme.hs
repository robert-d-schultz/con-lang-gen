{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Gen.Grapheme
( makeCharacters
, makeCharacter
) where

import ClassyPrelude
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)

import Data.Phoneme
import Data.Other
import Data.Inflection

-- note: bezier curves are a problem

-- simplest, make random squiggles and calls them chracters
makeCharacters :: ([(Phoneme, Int)], [(Syllable, Int)], [(((Text, LexCat), Morpheme), Int)]) -> RVar ([(Phoneme, (Int, [(Text,[(Int,Int)])]))], [(Syllable, (Int, [(Text,[(Int,Int)])]))], [(((Text, LexCat), Morpheme), (Int, [(Text,[(Int,Int)])]))])
makeCharacters ([], [], []) = return ([], [], [])
makeCharacters (a, s, l) = do
  aOut <- mapM (makeCharacter 11 . snd) a
  sOut <- mapM (makeCharacter 11 . snd) s
  lOut <- mapM (makeCharacter 11 . snd) l
  return (zipWith help a aOut, zipWith help s sOut, zipWith help l lOut)

help :: (a, b) -> c -> (a, (b,c))
help (x,y) z = (x,(y,z))

makeCharacter :: Int -> Int -> RVar [(Text,[(Int,Int)])]
makeCharacter n _ = do
  startPos <- sequence [(,) <$> (uniform 0 n :: RVar Int) <*> (uniform 0 n :: RVar Int)]
  stuff <- replicateM 3 (svgStuff n)
  let path = ("M", startPos):stuff
  let scaledPath = scaleSVG n path
  return $ scaledPath ++ [("Z",[(-1,-1)])]

scaleSVG :: Int -> [(Text,[(Int,Int)])] -> [(Text,[(Int,Int)])]
scaleSVG n old = new where
  maxX = maximumMay (filter (<=n) (concatMap (map fst . snd) old))
  minX = maximumMay (filter (>=0) (concatMap (map fst . snd) old))
  maxY = maximumMay (filter (<=n) (concatMap (map snd . snd) old))
  minY = maximumMay (filter (>=0) (concatMap (map snd . snd) old))

  maxX_ = fromMaybe n maxX
  minX_ = fromMaybe 0 minX
  maxY_ = fromMaybe n maxY
  minY_ = fromMaybe 0 minY

  diffX = maxX_- minX_
  diffY = maxY_ - minY_
  new = map (second (map (\(x,y) -> (round (realToFrac (x - minX_) * (realToFrac n / realToFrac diffX)), round (realToFrac (y - minY_) * (realToFrac n / realToFrac diffY)))))) old

svgStuff :: Int -> RVar (Text, [(Int,Int)])
svgStuff n = join $ choice [ (,) "L" <$> sequence [(,) <$> (uniform 0 n :: RVar Int) <*> (uniform 0 n :: RVar Int)] -- line to some point
                         , (,) "Q" <$> sequence [(,) <$> (uniform 0 n :: RVar Int) <*> (uniform 0 n :: RVar Int), (,) <$> (uniform 0 n :: RVar Int) <*> (uniform 0 n :: RVar Int)] -- bezier curve
                         , (,) "H" <$> sequence [(,) <$> (uniform 0 n :: RVar Int) <*> return (-1)] -- horizontal line
                         , (,) "V" <$> sequence [(,) <$> return (-1) <*> (uniform 0 n :: RVar Int)] -- vertical line
                         ]
