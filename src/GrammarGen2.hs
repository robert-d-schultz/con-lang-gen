module GrammarGen2
( makeGrammarSystem
, loadInputData2
) where

import Prelude hiding (Word)
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Control.Monad

import PhonemeInventoryGen2
import PhonemeType2
import PhonotacticsGen2
import OtherData2
import GrammarType2

-- Generates a framework from which declension can be made

data InputData = InputData
    {
      inputGender       :: [[Gender]]
    , inputAnimacy      :: [[Animacy]]
    , inputCase         :: [[Case]]
    , inputNumber       :: [[Number]]
    , inputHonorific    :: [[Honorific]]
    , inputDefiniteness  :: [[Definiteness]]
    , inputSpecificity  :: [[Specificity]]
    }

loadInputData2 :: IO InputData
loadInputData2 =
    InputData
        <$> readFeature "raw/grammatical categories/declension/gender.txt"
        <*> readFeature "raw/grammatical categories/declension/animacy.txt"
        <*> readFeature "raw/grammatical categories/declension/case.txt"
        <*> readFeature "raw/grammatical categories/declension/number.txt"
        <*> readFeature "raw/grammatical categories/declension/honorific.txt"
        <*> readFeature "raw/grammatical categories/declension/definiteness.txt"
        <*> readFeature "raw/grammatical categories/declension/specificity.txt"

readFeature :: Read a => FilePath -> IO a
readFeature = fmap read . readFile

-- Create "grammar system"
-- Decides if the category will manifest, and if it will manifest by particle or exponent
-- Also decides which categories will be grouped together
-- For instance, gender, case, and number as a single exponent (declension)
-- Or gender, case, and number each as an exponent (agglutination)
-- And anything in between
makeGrammarSystem :: InputData -> RVar (GrammarSystem, (Int,Int))
makeGrammarSystem idata = do
  (gSys, (gpn, gen)) <- fooGender idata
  (aSys, (apn, aen)) <- fooAnimacy idata (gpn, gen)
  (cSys, (cpn, cen)) <- fooCase idata (apn, aen)
  (nSys, (npn, nen)) <- fooNumber idata (cpn, cen)
  (hSys, (hpn, hen)) <- fooHonorific idata (npn, nen)
  (dSys, (dpn, den)) <- fooDefiniteness idata (hpn, hen)
  (sSys, (nPar, nExp)) <- fooSpecificity idata (dpn, den)
  return (GrammarSystem gSys aSys cSys nSys hSys dSys sSys, (nPar, nExp))

fooGender :: InputData -> RVar (Manifest ([Gender], Int), (Int, Int))
fooGender idata = do
  gs <- makeGenders idata
  choice [ (NoManifest, (0,0))
         , (Particle (gs, 1), (1, 0))
         , (Exponent (gs, 1), (0, 1))
         ]

fooAnimacy :: InputData -> (Int, Int) -> RVar (Manifest ([Animacy], Int), (Int, Int))
fooAnimacy idata (gpn, gen) = do
  as <- makeAnimacies idata
  apn <- uniform 1 (gpn + 1)
  aen <- uniform 1 (gen + 1)
  choice [ (NoManifest, (gpn, gen))
         , (Particle (as, apn), (max gpn apn,gen))
         , (Exponent (as, aen), (gpn,max gen aen))
         ]

fooCase :: InputData -> (Int, Int) -> RVar (Manifest ([Case], Int), (Int, Int))
fooCase idata (apn, aen) = do
  cs <- makeCases idata
  cpn <- uniform 1 (apn + 1)
  cen <- uniform 1 (aen + 1)
  choice [ (NoManifest, (apn, aen))
         , (Particle (cs, cpn), (max apn cpn, aen))
         , (Exponent (cs, cen), (apn, max aen cen))
         ]

fooNumber :: InputData -> (Int, Int) -> RVar (Manifest ([Number], Int), (Int, Int))
fooNumber idata (cpn, cen) = do
  ns <- makeNumbers idata
  npn <- uniform 1 (cpn + 1)
  nen <- uniform 1 (cen + 1)
  choice [ (NoManifest, (cpn, cen))
         , (Particle (ns, npn), (max cpn npn, cen))
         , (Exponent (ns, nen), (cpn, max cen nen))
         ]

fooHonorific :: InputData -> (Int, Int) -> RVar (Manifest ([Honorific], Int), (Int, Int))
fooHonorific idata (npn, nen) = do
  hs <- makeHonorifics idata
  hpn <- uniform 1 (npn + 1)
  hen <- uniform 1 (nen + 1)
  choice [ (NoManifest, (npn, nen))
         , (Particle (hs, hpn), (max npn hpn, nen))
         , (Exponent (hs, hen), (npn, max nen hen))
         ]

fooDefiniteness :: InputData -> (Int, Int) -> RVar (Manifest ([Definiteness], Int), (Int, Int))
fooDefiniteness idata (hpn, hen) = do
  ds <- makeDefinitenesses idata
  dpn <- uniform 1 (hpn + 1)
  den <- uniform 1 (hen + 1)
  choice [ (NoManifest, (hpn, hen))
         , (Particle (ds, dpn), (max hpn dpn, hen))
         , (Exponent (ds, den), (hpn, max hen den))
         ]

fooSpecificity :: InputData -> (Int, Int) -> RVar (Manifest ([Specificity], Int), (Int, Int))
fooSpecificity idata (dpn, den) = do
  ss <- makeSpecificities idata
  spn <- uniform 1 (dpn + 1)
  sen <- uniform 1 (den + 1)
  choice [ (NoManifest, (dpn, den))
         , (Particle (ss, spn), (max dpn spn, den))
         , (Exponent (ss, sen), (dpn, max den sen))
         ]

-- Will expand these eventually
makeGenders :: InputData -> RVar [Gender]
makeGenders idata = choice $ inputGender idata

makeAnimacies :: InputData -> RVar [Animacy]
makeAnimacies idata = choice $ inputAnimacy idata

makeCases :: InputData -> RVar [Case]
makeCases idata = choice $ inputCase idata

makeNumbers :: InputData -> RVar [Number]
makeNumbers idata = choice $ inputNumber idata

makeHonorifics :: InputData -> RVar [Honorific]
makeHonorifics idata = choice $ inputHonorific idata

makeDefinitenesses :: InputData -> RVar [Definiteness]
makeDefinitenesses idata = choice $ inputDefiniteness idata

makeSpecificities ::  InputData -> RVar [Specificity]
makeSpecificities idata = choice $ inputSpecificity idata
