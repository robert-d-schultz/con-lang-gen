{-# OPTIONS_GHC -Wall #-}
module Morph.Grammar
( morphGrammar
) where

import Data.RVar

import HelperFunctions

import Data.Grammar

-- change grammatical parameter
morphGrammar :: Grammar -> RVar Grammar
morphGrammar (Grammar subI objF comI vtoI affH nulS oblT nulT topM itoC whM pieP queI) = do
  let n = 5
  subIN <- choice_ n subI (swap subI)
  objFN <- choice_ n objF (swap objF)
  comIN <- choice_ n comI (swap comI)

  affHN <- choice_ n affH (swap affH)
  vtoIN <- if affHN == NoAffixHop then choice_ n vtoI (swap vtoI) else return vtoI

  oblTN <- choice_ n oblT (swap oblT)
  nulSN <- choice_ n nulS (swap nulS)
  nulTN <- if oblTN == OblTopic && nulSN == NoNullSub then choice_ n nulT (swap nulT) else return nulT

  topMN <- choice_ n topM (swap topM)
  itoCN <- choice_ n itoC (swap itoC)
  whMN  <- choice_ n whM (swap whM)
  piePN <- choice_ n pieP (swap pieP)
  queIN <- choice_ n queI (swap queI)

  return $ Grammar subIN objFN comIN vtoIN affHN nulSN oblTN nulTN topMN itoCN whMN piePN queIN

swap :: Eq a => Bounded a => a -> a
swap a
 | a == maxBound = minBound
 | a == minBound = maxBound
 | otherwise = a
