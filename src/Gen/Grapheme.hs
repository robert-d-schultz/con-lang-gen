module Gen.Grapheme
( makeCharacters
) where

import ClassyPrelude
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)

import Data.Phoneme
import Data.Other
import Data.Inflection

import HelperFunctions

-- note: bezier curves are a problem

-- simplest, make random squiggles and call them chracters
makeCharacters :: ([(Phoneme, Int)], [(Syllable, Int)], [(((Text, LexCat), SyllWord), Int)]) -> RVar ([(Phoneme, (Int, CharPath))], [(Syllable, (Int, CharPath))], [(((Text, LexCat), SyllWord), (Int, CharPath))])
makeCharacters ([], [], []) = return ([], [], [])
makeCharacters (a, s, l) = do
  let n = 11 -- arbitrary character "size"
  rads <- makeRadicals n
  yStart <- uniform 0 n
  aOut <- replicateM (length a) (makeCharFromRads n yStart rads)
  sOut <- replicateM (length s) (makeCharFromRads n yStart rads)
  lOut <- replicateM (length l) (makeCharFromRads n yStart rads)
  return (zipWith help a aOut, zipWith help s sOut, zipWith help l lOut)

help :: (a, b) -> c -> (a, (b,c))
help (x,y) z = (x,(y,z))

makeCharFromRads :: Int -> Int -> [RadPath] -> RVar CharPath
makeCharFromRads n ySt rads = do
  chosenRads <- choices2 2 rads --arbitray amount
  let path = concat chosenRads
  let path_ = convertSVG (0, ySt) path
  let scaledPath = scaleSVG n path_
  return $ Mov Abs (-1) ySt : Hor Abs 0 : scaledPath ++ [Lin Abs n ySt, Hor Abs (n+1)]


makeRadPatterns :: RadPath -> [RadPath]
makeRadPatterns rad = [ rad
                      , reverse rad
                      , mirYRadPath rad
                      , reverse $ mirYRadPath rad
                      ]

-- Radical here refers to a "pattern" used across many characters
makeRadicals :: Int -> RVar [RadPath]
makeRadicals n = do
  rs <- replicateM 3 (makeRadical n) --arbitray amount
  let rs_ = map makeRadPatterns rs :: [[RadPath]]
  return (concat rs_ :: [RadPath])

makeRadical :: Int -> RVar RadPath
makeRadical n = do
  path <- replicateM 3 (svgStuff n) --arbitray amount
  return path

-- Mirror radicals and stuff
mirXRadPath :: RadPath -> RadPath
mirXRadPath ms = map mirX ms

mirX :: SVGMove -> SVGMove
mirX (Mov Rel x y) = Mov Rel (-x) y
mirX (Lin Rel x y) = Lin Rel (-x) y
mirX (Qur Rel cpx cpy x y) = Qur Rel (-cpx) cpy (-x) y
mirX (Hor Rel x) = Hor Rel (-x)
mirX m = m

mirYRadPath :: RadPath -> RadPath
mirYRadPath ms = map mirY ms

mirY :: SVGMove -> SVGMove
mirY (Mov Rel x y) = Mov Rel x (-y)
mirY (Lin Rel x y) = Lin Rel x (-y)
mirY (Qur Rel cpx cpy x y) = Qur Rel cpx (-cpy) x (-y)
mirY (Ver Rel y) = Ver Rel (-y)
mirY m = m

mirXYRadPath :: RadPath -> RadPath
mirXYRadPath = mirXRadPath.mirYRadPath

-- Scale SVG into bounding box
scaleSVG :: Int -> [SVGMove] -> [SVGMove]
scaleSVG n ms = new where
  xs = concatMap getXSVG ms
  ys = concatMap getYSVG ms
  maxX = maximumMay xs
  minX = minimumMay xs
  maxY = maximumMay ys
  minY = minimumMay ys

  maxX_ = fromMaybe n maxX
  minX_ = fromMaybe 0 minX
  maxY_ = fromMaybe n maxY
  minY_ = fromMaybe 0 minY

  diffX = maxX_ - minX_
  diffY = maxY_ - minY_

  scaleX x = round (realToFrac (x - minX_) * (realToFrac n / realToFrac diffX))
  scaleY y = round (realToFrac (y - minY_) * (realToFrac n / realToFrac diffY))
  new = map (scaleXSVG scaleX.scaleYSVG scaleY) ms

getXSVG :: SVGMove -> [Int]
getXSVG m@Qur{} = [getX m, getCPX m]
getXSVG Ver{} = []
getXSVG Znd = []
getXSVG m = [getX m]

getYSVG :: SVGMove -> [Int]
getYSVG m@Qur{} = [getY m, getCPY m]
getYSVG Hor{} = []
getYSVG Znd = []
getYSVG m = [getY m]

scaleXSVG :: (Int -> Int) -> SVGMove -> SVGMove
scaleXSVG f (Qur ar cpx cpy x y) = Qur ar (f cpx) cpy (f x) y
scaleXSVG _ m@Ver{} = m
scaleXSVG _ Znd = Znd
scaleXSVG f m = m{getX = f (getX m)}

scaleYSVG :: (Int -> Int) -> SVGMove -> SVGMove
scaleYSVG f (Qur ar cpx cpy x y) = Qur ar cpx (f cpy) x (f y)
scaleYSVG _ m@Hor{} = m
scaleYSVG _ Znd = Znd
scaleYSVG f m = m{getY = f (getY m)}

-- convert SVGMove's from relative to absolute
convertSVG :: (Int, Int) -> [SVGMove] -> [SVGMove]
convertSVG _ [] = []
convertSVG (x,y) ms = fromMaybe [] out where
  out = do
          a <- headMay ms
          let (b, coord) = toAbs (x,y) a
          t <- tailMay ms
          return $ b : convertSVG coord t

svgStuff :: Int -> RVar SVGMove
svgStuff n = join $ choice [ Lin Rel <$> (uniform 0 2 :: RVar Int) <*> (uniform (-2) 2 :: RVar Int) -- line to some point
                           , Qur Rel <$> (uniform 0 2 :: RVar Int) <*> (uniform (-2) 2 :: RVar Int) <*> (uniform 0 2 :: RVar Int) <*> (uniform (-2) 2 :: RVar Int) -- bezier curve
                           , Hor Rel <$> (uniform 0 2 :: RVar Int) -- horizontal line
                           , Ver Rel <$> (uniform (-2) 2 :: RVar Int) -- vertical line
                           ]


toAbs :: (Int,Int) -> SVGMove -> (SVGMove, (Int, Int))
toAbs (x,y) Znd = (Znd, (x,y))
toAbs (x,y) b@Qur{} = (b{getAR = Abs, getCPX = x + getCPX b, getCPY = y + getCPY b, getX = x + getX b, getY = y + getY b}, (x + getX b,y + getY b))
toAbs (x,y) b@Hor{} = (b{getAR = Abs, getX = x + getX b}, (x + getX b,y))
toAbs (x,y) b@Ver{} = (b{getAR = Abs, getY = y + getY b}, (x,y + getY b))
toAbs (x,y) b = (b{getAR = Abs, getX = x + getX b, getY = x + getY b}, (x + getX b,y + getY b))
