module Gen.Grapheme
( makeCharacters
) where

import ClassyPrelude
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)

import Data.Phoneme
import Data.Word
import Data.Other
import Data.Inflection

import HelperFunctions

-- note: bezier curves are a problem

-- Make random squiggles
makeCharacters :: ([(Phoneme, Int)], [(Syllable, Int)], [(Morpheme, Int)]) -> RVar ([(Phoneme, (Int, CharPath))], [(Syllable, (Int, CharPath))], [(Morpheme, (Int, CharPath))])
makeCharacters ([], [], []) = return ([], [], [])
makeCharacters (a, s, l) = do
  let n = 51 :: Float -- arbitrary character "size"
  rads <- makeRadicals
  yStart <- uniform 0 51 :: RVar Float
  aOut <- replicateM (length a) (makeCharFromRads n yStart rads)
  sOut <- replicateM (length s) (makeCharFromRads n yStart rads)
  lOut <- replicateM (length l) (makeCharFromRads n yStart rads)
  return (zipWith help a aOut, zipWith help s sOut, zipWith help l lOut)

help :: (a, b) -> c -> (a, (b,c))
help (x,y) z = (x,(y,z))

makeCharFromRads :: Float -> Float -> [RadPath] -> RVar CharPath
makeCharFromRads n ySt rads = join $ choice <$> sequence [makeHorCharFromRads n ySt rads, makeVertCharFromRads n ySt rads]

makeHorCharFromRads :: Float -> Float -> [RadPath] -> RVar CharPath
makeHorCharFromRads n ySt rads = do
  chosenRads <- choices2 2 rads --arbitrary amount
  let path = concat chosenRads
  let path_ = convertSVG (0, ySt) path
  let scaledPath = scaleSVG n ySt (0,0) path_
  return $ Mov Abs (-1) ySt : Hor Abs 0 : scaledPath ++ [Lin Abs n ySt, Hor Abs (n+1)]

makeVertCharFromRads :: Float -> Float -> [RadPath] -> RVar CharPath
makeVertCharFromRads n ySt rads
  | ySt < n / 3 = below
  | ySt > n - n / 3 = above
  | otherwise = join $ choice <$> sequence [above, below] where
  above = do
    chosenRads <- choices2 2 rads --arbitrary amount
    xStart <- uniform 0 n
    let path = (rotate90RadPath.mirXRadPath) (concat chosenRads)
    let path_ = convertSVG (0, ySt) path
    let offset = ySt / 5
    let scaledPath = scaleSVG n ySt (ySt-offset, 0) path_
    return $ Mov Abs (-1) ySt : Hor Abs xStart : Ver Abs (ySt-offset) : scaledPath ++ [Hor Abs xStart, Ver Abs ySt] ++ [Hor Abs (n+1)]
  below = do
    chosenRads <- choices2 2 rads --arbitrary amount
    xStart <- uniform 0 n
    let path = rotate90RadPath (concat chosenRads)
    let path_ = convertSVG (0, ySt) path
    let offset = (n - ySt) / 5
    let scaledPath = scaleSVG n ySt (0, ySt+offset) path_
    return $ Mov Abs (-1) ySt : Hor Abs xStart : Ver Abs (ySt+offset) : scaledPath ++ [Hor Abs xStart, Ver Abs ySt] ++ [Hor Abs (n+1)]

-- Radical here refers to a "pattern" used across many characters
makeRadicals :: RVar [RadPath]
makeRadicals = do
  rs <- replicateM 6 makeRadical --arbitrary amount
  let rs_ = map makeRadPatterns rs :: [[RadPath]]
  return (concat rs_ :: [RadPath])

makeRadical :: RVar RadPath
makeRadical = do
  rad <- replicateM 3 svgStuff --arbitrary amount
  -- reject radicals with two consecutive straight lines
  let a = groupBy (\x y -> all (\case Ver{} ->  True; _ -> False) [x,y] || all (\case Hor{} ->  True; _ -> False) [x,y]) rad
  if any (\z -> length z > 1) a then makeRadical else return rad

makeRadPatterns :: RadPath -> [RadPath]
makeRadPatterns rad = [ rad
                      , reverse rad
                      , mirYRadPath rad
                      , reverse $ mirYRadPath rad
                      ]

-- Mirror radicals and stuff
mirXRadPath :: RadPath -> RadPath
mirXRadPath = map mirX

mirX :: SVGMove -> SVGMove
mirX (Mov Rel x y) = Mov Rel (-x) y
mirX (Lin Rel x y) = Lin Rel (-x) y
mirX (Hor Rel x) = Hor Rel (-x)
mirX (Cir ar r larc sweep x y) = Cir ar r larc sweep (-x) y
--mirX (Qur Rel cpx cpy x y) = Qur Rel (-cpx) cpy (-x) y
mirX m = m

mirYRadPath :: RadPath -> RadPath
mirYRadPath = map mirY

mirY :: SVGMove -> SVGMove
mirY (Mov Rel x y) = Mov Rel x (-y)
mirY (Lin Rel x y) = Lin Rel x (-y)
mirY (Ver Rel y) = Ver Rel (-y)
mirY (Cir ar r larc sweep x y) = Cir ar r larc sweep x (-y)
--mirY (Qur Rel cpx cpy x y) = Qur Rel cpx (-cpy) x (-y)
mirY m = m

mirXYRadPath :: RadPath -> RadPath
mirXYRadPath = mirXRadPath.mirYRadPath

rotate90RadPath :: RadPath -> RadPath
rotate90RadPath = map rotate90

rotate90 :: SVGMove -> SVGMove
rotate90 (Mov ar x y) = Mov ar y x
rotate90 (Lin ar x y) = Lin ar y x
rotate90 (Hor ar x) = Ver ar x
rotate90 (Ver ar y) = Hor ar y
rotate90 (Cir ar r larc sweep x y) = Cir ar r larc sweep y x
--rotate90 (Qur ar cpx cpy x y) = Qur ar cpy cpx y x
rotate90 Znd = Znd

-- Scale SVG into bounding box
scaleSVG :: Float -> Float -> (Float, Float) -> [SVGMove] -> [SVGMove]
scaleSVG n ySt (foo,bar) ms = new where
  (minX, maxX, minY, maxY) = bounding (0,ySt) (n,0,n,0) ms

  maxX_ = maxX
  minX_ = minX
  maxY_ = maxY + foo
  minY_ = minY - bar

  diffX = maxX_ - minX_
  diffY = maxY_ - minY_

  scaleX x = (x - minX_) * (n / diffX)
  scaleY y = (y - minY_) * (n / diffY)
  new = map (scaleXSVG scaleX.scaleYSVG scaleY) ms

scaleXSVG :: (Float -> Float) -> SVGMove -> SVGMove
--scaleXSVG f (Qur ar cpx cpy x y) = Qur ar (f cpx) cpy (f x) y
scaleXSVG f (Cir ar r larc sweep x y) = Cir ar (f r) larc sweep (f x) y
scaleXSVG f (m@Hor{}) = m{getX = f (getX m)}
scaleXSVG f (m@Ver{}) = m
scaleXSVG _ Znd = Znd
scaleXSVG f m = m{getX = f (getX m)}

scaleYSVG :: (Float -> Float) -> SVGMove -> SVGMove
--scaleYSVG f (Qur ar cpx cpy x y) = Qur ar cpx (f cpy) x (f y)
scaleYSVG f (Cir ar r larc sweep x y) = Cir ar r larc sweep x (f y)
scaleYSVG f (m@Hor{}) = m
scaleYSVG f (m@Ver{}) = m{getY = f (getY m)}
scaleYSVG _ Znd = Znd
scaleYSVG f m = m{getY = f (getY m)}

-- convert SVGMove's from relative to absolute
convertSVG :: (Float, Float) -> [SVGMove] -> [SVGMove]
convertSVG _ [] = []
convertSVG (x,y) ms = fromMaybe [] out where
  out = do
          a <- headMay ms
          let (b, coord) = toAbs (x,y) a
          t <- tailMay ms
          return $ b : convertSVG coord t

svgStuff :: RVar SVGMove
svgStuff = join $ choice [ uncurry (Lin Rel) <$> choice ((,) <$> [1,2] <*> [-2,-1,1,2])
                         -- , Qur Rel <$> (uniform 0 2 :: RVar Int) <*> (uniform (-2) 2 :: RVar Int) <*> (uniform 1 2 :: RVar Int) <*> (uniform (-2) 2 :: RVar Int) -- bezier curve
                         , Hor Rel <$> (choice [1,2] :: RVar Float) -- horizontal line
                         , Ver Rel <$> (choice [-2,-1,1,2] :: RVar Float) -- vertical line
                         , Cir Rel <$> (choice [1,2] :: RVar Float) -- circle
                                   <*> (choice [0,1] :: RVar Int)
                                   <*> (choice [0,1] :: RVar Int)
                                   <*> (choice [1,2] :: RVar Float)
                                   <*> (uniform (-2) 2 :: RVar Float)
                         ]


-- Switches from relative movement to absolute
toAbs :: (Float, Float) -> SVGMove -> (SVGMove, (Float, Float))
toAbs (x,y) Znd = (Znd, (x,y))
{-toAbs (x,y) b@Qur{}
  | getAR b == Abs = (b, (x,y))
  | otherwise = (b{getAR = Abs, getCPX = x + getCPX b, getCPY = y + getCPY b, getX = x + getX b, getY = y + getY b}, (x + getX b,y + getY b))-}
toAbs (x,y) b@Cir{}
  | getAR b == Abs = (b, (x,y))
  | otherwise = (b{getAR = Abs, getX = x + getX b, getY = y + getY b}, (x + getX b,y + getY b))
toAbs (x,y) b@Hor{}
  | getAR b == Abs = (b, (x,y))
  | otherwise = (b{getAR = Abs, getX = x + getX b}, (x + getX b,y))
toAbs (x,y) b@Ver{}
  | getAR b == Abs = (b, (x,y))
  | otherwise = (b{getAR = Abs, getY = y + getY b}, (x,y + getY b))
toAbs (x,y) b
  | getAR b == Abs = (b, (x,y))
  | otherwise = (b{getAR = Abs, getX = x + getX b, getY = y + getY b}, (x + getX b,y + getY b))


bounding :: (Float, Float) -> (Float, Float, Float, Float) -> [SVGMove] -> (Float, Float, Float, Float)
bounding _ b [] = b
bounding (x1,y1) b@(xMin, xMax, yMin, yMax) (Cir _ r larc sweep x2 y2:ms) = bounding (x2,y2) b_ ms where
  (xMin_, xMax_, yMin_, yMax_) = boundingEllipse x1 y1 r r 0 larc sweep x2 y2
  b_ = (min xMin xMin_, max xMax xMax_, min yMin yMin_, max yMax yMax_)
bounding (x,y) b (Znd:ms) = bounding (x,y) b ms
bounding (x,y) b@(xMin, xMax, yMin, yMax) ((m@Hor{}):ms) = bounding (getX m,y) (min xMin (getX m), max xMax (getX m), yMin, yMax) ms
bounding (x,y) b@(xMin, xMax, yMin, yMax) ((m@Ver{}):ms) = bounding (x,getY m) (xMin, xMax, min yMin (getY m), max yMax (getY m)) ms
bounding (x,y) b@(xMin, xMax, yMin, yMax) (m:ms) = bounding (getX m, getY m) (min xMin (getX m), max xMax (getX m), min yMin (getY m), max yMax (getY m)) ms

-- sorta, does box around entire ellipse
boundingEllipse :: Float -> Float -> Float -> Float -> Float -> Int -> Int -> Float -> Float -> (Float, Float, Float, Float)
boundingEllipse x1 y1 0 0 _ _ _ x2 y2 = (min x1 x2, max x1 x2, min y1 y2, max y1 y2)
boundingEllipse x1 y1 rx__ ry__ rot larc sweep x2 y2 = out where
  x1' = cos rot * (x1 - x2)/2 + sin rot * (y1 - y2)/2
  y1' = -sin rot * (x1 - x2)/2 + cos rot * (y1 - y2)/2
  rx_ = abs rx__
  ry_ = abs ry__
  lam = x1'^2/rx_^2 + y1'^2/ry_^2
  (rx, ry)
    | lam <= 1 = (abs rx_, abs ry_)
    | otherwise = (sqrt lam * rx_, sqrt lam * ry_)
  radicant = (rx^2 * ry^2 - rx^2 * y1'^2 - ry^2 * x1'^2)
           / (rx^2 * y1'^2 + ry^2*x1'^2)
  sign = if larc /= sweep then 1 else -1
  cx' = sign * radicant * rx * y1' / ry
  cy' = sign * radicant * (-ry) * x1' / rx
  cx = cos rot * cx' + (-sin rot * cy') + (x1 + x2)/2
  cy = sin rot * cx' + cos rot * cy' + (y1 + y2)/2
  out = (cx - rx, cx + rx, cy - ry, cy + ry)
