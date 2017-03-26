module Gen.Grapheme
( makeCharacters
, makeCharacter
) where

import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Data.List
import Control.Monad
import Control.Arrow

import Data.Phoneme
import Data.Other
import Data.Inflection

-- note: bezier curves are a problem

-- simplest, make random squiggles and calls them chracters
makeCharacters :: ([(Phoneme, Int)], [(Syllable, Int)], [(((String, LexCat), Morpheme), Int)]) -> RVar ([(Phoneme, (Int, [(String,[(Int,Int)])]))], [(Syllable, (Int, [(String,[(Int,Int)])]))], [(((String, LexCat), Morpheme), (Int, [(String,[(Int,Int)])]))])
makeCharacters ([], [], []) = return ([], [], [])
makeCharacters (a, s, l) = do
  aOut <- mapM (makeCharacter 11 1 . snd) a
  sOut <- mapM (makeCharacter 11 1 . snd) s
  lOut <- mapM (makeCharacter 11 1 . snd) l
  return (zipWith help a aOut, zipWith help s sOut, zipWith help l lOut)

help :: (a, b) -> c -> (a, (b,c))
help (x,y) z = (x,(y,z))

makeCharacter :: Int -> Int ->  Int -> RVar [(String,[(Int,Int)])]
makeCharacter n w _ = do
  startPos <- sequence [(,) <$> (uniform 0 n :: RVar Int) <*> (uniform 0 n :: RVar Int)]
  stuff <- replicateM 3 (svgStuff n)
  let path = ("M", startPos):stuff
  let scaledPath = scaleSVG n path
  return $ scaledPath ++ [("Z",[(-1,-1)])]

scaleSVG :: Int -> [(String,[(Int,Int)])] -> [(String,[(Int,Int)])]
scaleSVG n old = new where
  maxX = maximum (filter (<=n) (concatMap (map fst . snd) old))
  minX = minimum (filter (>=0) (concatMap (map fst . snd) old))
  maxY = maximum (filter (<=n) (concatMap (map snd . snd) old))
  minY = minimum (filter (>=0) (concatMap (map snd . snd) old))
  diffX = maxX - minX
  diffY = maxY - minY
  new = map (second (map (\(x,y) -> (round (realToFrac (x - minX) * (realToFrac n / realToFrac diffX)), round (realToFrac (y - minY) * (realToFrac n / realToFrac diffY)))))) old

svgStuff :: Int -> RVar (String, [(Int,Int)])
svgStuff n = join $ choice [ (,) "L" <$> sequence [(,) <$> (uniform 0 n :: RVar Int) <*> (uniform 0 n :: RVar Int)] -- line to some point
                         , (,) "Q" <$> sequence [(,) <$> (uniform 0 n :: RVar Int) <*> (uniform 0 n :: RVar Int), (,) <$> (uniform 0 n :: RVar Int) <*> (uniform 0 n :: RVar Int)] -- bezier curve
                         , (,) "H" <$> sequence [(,) <$> (uniform 0 n :: RVar Int) <*> return (-1)] -- horizontal line
                         , (,) "V" <$> sequence [(,) <$> return (-1) <*> (uniform 0 n :: RVar Int)] -- vertical line
                         ]
