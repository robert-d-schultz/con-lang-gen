module Data.Other
( AbsRel(..)
, SVGMove(..)
, RadPath
, CharPath
) where

import ClassyPrelude

import Data.Phoneme
import Data.Soundchange

-- used for graphemes and writing systems
type RadPath = [SVGMove]
type CharPath = [SVGMove]

data AbsRel = Abs | Rel deriving (Eq)

data SVGMove = Mov {getAR :: AbsRel, getX :: Float, getY :: Float}
             | Lin {getAR :: AbsRel, getX :: Float, getY :: Float}
             -- | Qur {getAR :: AbsRel, getCPX :: Float , getCPY :: Float, getX :: Float, getY :: Float}
             | Hor {getAR :: AbsRel, getX :: Float}
             | Ver {getAR :: AbsRel, getY :: Float}
             | Cir {getAR :: AbsRel, getR :: Float, getLArc :: Int, getSweep :: Int, getX :: Float, getY :: Float}
             | Znd

instance Show SVGMove where
  show (Mov Abs x y) = "M" ++ show x ++ " " ++ show y
  show (Lin Abs x y) = "L" ++ show x ++ " " ++ show y
  --show (Qur Abs cpx cpy x y) = "Q" ++ unwords (map show [cpx, cpy, x, y])
  show (Hor Abs x) = "H" ++ show x
  show (Ver Abs y) = "V" ++ show y
  show (Cir Abs r larc sweep x y) = "A" ++ unwords [show r, show r, show 0, show larc, show sweep, show x, show y]
  show (Mov Rel x y) = "m" ++ show x ++ " " ++ show y
  show (Lin Rel x 0) = "h" ++ show x
  show (Lin Rel 0 y) = "v" ++ show y
  show (Lin Rel x y) = "l" ++ show x ++ " " ++ show y
  --show (Qur Rel cpx cpy x y) = "q" ++ unwords (map show [cpx, cpy, x, y])
  show (Hor Rel x) = "h" ++ show x
  show (Ver Rel y) = "v" ++ show y
  show (Cir Rel r larc sweep x y) = "a" ++ unwords [show r, show r, show 0, show larc, show sweep, show x, show y]
  show Znd = "Z"
