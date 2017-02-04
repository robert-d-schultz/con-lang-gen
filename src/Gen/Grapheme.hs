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

-- simplest, make random squiggles and calls them chracters
makeCharacters :: Int -> RVar [String]
makeCharacters 0 = return []
makeCharacters n = (:) <$> makeCharacter <*> makeCharacters (n-1)

makeCharacter :: RVar String
makeCharacter = do
  startPos <- sequence [(,) <$> (uniform 0 100 :: RVar Int) <*> (uniform 0 100 :: RVar Int)]
  stuff <- replicateM 5 svgStuff
  let path = ("M",startPos):stuff
  let scaledPath = scaleSVG path
  let textPath = pathToText scaledPath
  end <- choice ["Z", ""]
  let path = "<path d=\""++ textPath ++ end ++ "\" stroke=\"black\" stroke-width=\"5\" fill=\"none\"/>"
  return ("<svg x=\"0\" y=\"0\" width=\"103\" height=\"103\" viewBox=\"0, 0, 103, 103\">" ++ path ++ "</svg>")

pathToText :: [(String,[(Int,Int)])] -> String
pathToText old = concat out where
  out = map (\(str, ints) -> str ++ concatMap (\(x,y) -> (if x >= 0 then show x ++ " " else "") ++ (if y >= 0 then show y ++ " " else "")) ints) old

scaleSVG :: [(String,[(Int,Int)])] -> [(String,[(Int,Int)])]
scaleSVG old = new where
  maxX = maximum (filter (<=100) (concatMap (map fst . snd) old))
  minX = minimum (filter (>=0) (concatMap (map fst . snd) old))
  maxY = maximum (filter (<=100) (concatMap (map snd . snd) old))
  minY = minimum (filter (>=0) (concatMap (map snd . snd) old))
  diffX = maxX - minX
  diffY = maxY - minY
  new = map (second (map (\(x,y) -> (round (realToFrac (x - minX) * (realToFrac 100 / realToFrac diffX)), round (realToFrac (y - minY) * (realToFrac 100 / realToFrac diffY)))))) old

svgStuff :: RVar (String, [(Int,Int)])
svgStuff = join $ choice [ (,) "L" <$> sequence [(,) <$> (uniform 0 100 :: RVar Int) <*> (uniform 0 100 :: RVar Int)] -- line to some point
                         , (,) "Q" <$> sequence [(,) <$> (uniform 0 100 :: RVar Int) <*> (uniform 0 100 :: RVar Int), (,) <$> (uniform 0 100 :: RVar Int) <*> (uniform 0 100 :: RVar Int)] -- bezier curve
                         , (,) "H" <$> sequence [(,) <$> (uniform 0 100 :: RVar Int) <*> return (-1)] -- horizontal line
                         , (,) "V" <$> sequence [(,) <$> return (-1) <*> (uniform 0 100 :: RVar Int)] -- vertical line
                         ]
