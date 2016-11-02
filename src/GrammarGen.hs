module GrammarGen
( makeGrammar
) where

import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)

import GrammarData

-- Generate grammar system for a language
makeGrammar :: RVar Grammar
makeGrammar = do
  subI <- choice [SubInitial, SubFinal]
  objF <- choice [ObjFinal, ObjInitial]
  comI <- choice [CompInitial, CompFinal]
  (vtoI, affH) <- choice $ (OblVtoIMove, NoAffixHop) : ((,) <$> [NoVtoIMove] <*> [NoAffixHop, OblAffixHop])
  (oblT, nulS, nulT) <- choice $ (OblTopic, NoNullSub, NoNullTop) : ((,,) <$> [OblTopic, OptTopic] <*> [NoNullSub, OptNullSub] <*> [OptNullTop])
  topM <- choice [NoTopMark, OblTopMark]
  itoC <- choice [NoItoCMove, OblItoCMove]
  whM  <- choice [NoWHMove, OblWHMove]
  pieP <- choice [PiedPipe, PrepStrand]
  queI <- choice [NoQuesInv, OblQuesInv]
  return $ Grammar subI objF comI vtoI affH nulS oblT nulT topM itoC whM pieP queI
