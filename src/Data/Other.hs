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

data SVGMove = Mov {getAR :: AbsRel, getX :: Int, getY :: Int}
             | Lin {getAR :: AbsRel, getX :: Int, getY :: Int}
             | Qur {getAR :: AbsRel, getCPX :: Int , getCPY :: Int, getX :: Int, getY :: Int}
             | Hor {getAR :: AbsRel, getX :: Int}
             | Ver {getAR :: AbsRel, getY :: Int}
             | Znd

instance Show SVGMove where
  show (Mov Abs x y) = "M" ++ show x ++ " " ++ show y
  show (Lin Abs x y) = "L" ++ show x ++ " " ++ show y
  show (Qur Abs cpx cpy x y) = "Q" ++ show cpx ++ " " ++ show cpy ++ " " ++ show x ++ " " ++ show y
  show (Hor Abs x) = "H" ++ show x
  show (Ver Abs y) = "V" ++ show y
  show (Mov Rel x y) = "m" ++ show x ++ " " ++ show y
  show (Lin Rel x y) = "l" ++ show x ++ " " ++ show y
  show (Qur Rel cpx cpy x y) = "q" ++ show cpx ++ " " ++ show cpy ++ " " ++ show x ++ " " ++ show y
  show (Hor Rel x) = "h" ++ show x
  show (Ver Rel y) = "v" ++ show y
  show Znd = "Z"
