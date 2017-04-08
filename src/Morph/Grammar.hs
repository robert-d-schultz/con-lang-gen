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
  subIN <- choice_ subI (swap subI) n
  objFN <- choice_ objF (swap objF) n
  comIN <- choice_ comI (swap comI) n

  affHN <- choice_ affH (swap affH) n
  vtoIN <- if affHN == NoAffixHop then choice_ vtoI (swap vtoI) n else return vtoI

  oblTN <- choice_ oblT (swap oblT) n
  nulSN <- choice_ nulS (swap nulS) n
  nulTN <- if oblTN == OblTopic && nulSN == NoNullSub then choice_ nulT (swap nulT) n else return nulT

  topMN <- choice_ topM (swap topM) n
  itoCN <- choice_ itoC (swap itoC) n
  whMN  <- choice_ whM (swap whM) n
  piePN <- choice_ pieP (swap pieP) n
  queIN <- choice_ queI (swap queI) n

  return $ Grammar subIN objFN comIN vtoIN affHN nulSN oblTN nulTN topMN itoCN whMN piePN queIN

swap :: Eq a => Bounded a => a -> a
swap a
 | a == maxBound = minBound
 | a == minBound = maxBound
