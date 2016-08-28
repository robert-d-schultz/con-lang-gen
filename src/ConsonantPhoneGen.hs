{-# LANGUAGE ScopedTypeVariables #-}
module ConsonantPhoneGen
(randomValue
) where

-- Import
import Prelude
import Data.RVar
--import Control.Monad
--import Data.Random hiding (sample)
--import qualified Data.Random.Sample as Sampler
--import Data.Random.Extras hiding (shuffle)
--import Data.Random.Source.IO
--import Data.Functor
--import Data.List
--import Control.Applicative
-- System.IO

-- Data structures
import PhonemeType

-- Picks a random value of an Enum data type
randomValue :: forall t. (Bounded t, Enum t) => RVar t
randomValue = toEnum <$> (uniform (fromEnum (minBound :: t)) (fromEnum (maxBound :: t)))


-- Generates a completely random consonant
-- Ignores consonant contours, for the moment
generateConsonant :: RVar ConsonantFeaturesContour
generateConsonant = do
    let possiblePlaces = [Place (Specified LOWERLIP) (Specified UPPERLIP)
                        , Place (Specified LOWERLIP) (Specified UPPERTEETH)
                        , Place (Specified TONGUEBLADE) (Specified UPPERLIP)
                        , Place (Specified TONGUEBLADE) (Specified UPPERTEETH)
                        , Place (Specified TONGUEBLADE) (Specified TEETHRIDGE)
                        , Place (Specified TONGUEBLADE) (Specified RIDGE)
                        , Place (Specified TONGUEBLADE) (Specified BACKRIDGE)
                        , Place (Specified TONGUETIP) (Specified UPPERTEETH)
                        , Place (Specified TONGUETIP) (Specified RIDGE)
                        , Place (Specified TONGUETIP) (Specified BACKRIDGE)
                        , Place (Specified TONGUEUNDER) (Specified HARDPALATE)
                        , Place (Specified TONGUEBODY) (Specified BACKRIDGE)
                        , Place (Specified TONGUEBODY) (Specified HARDPALATE)
                        , Place (Specified TONGUEBODY) (Specified SOFTPALATE)
                        , Place (Specified TONGUEBODY) (Specified UVULA)
                        , Place (Specified TONGUEROOT) (Specified PHARYNX)
                        , Place (Specified LARYNX) (Specified PHARYNX)
                        , Place (Specified LARYNX) (Specified EPIGLOTTIS)
                        , Place (Specified LARYNX) (Specified GLOTTIS)
                        ]
    place1 <- choice possiblePlaces

    initiatorG <- randomValue :: RVar Initiator
    directionG <- randomValue :: RVar Direction
    let airstream1 = Airstream (Specified initiatorG) (Specified directionG)

    phonationG <- randomValue :: RVar Phonation

    strictureG <- randomValue :: RVar Stricture
    lengthG <- randomValue :: RVar Length
    airescapeG <- randomValue :: RVar AirEscape

    r1 <- randomValue :: RVar Trill
    let trillG
            | (elem (passive (place1)) [Specified GLOTTIS, Specified BACKRIDGE, Specified HARDPALATE, Specified SOFTPALATE]) = Impossible
            | otherwise = Possible (Specified r1)


    r2 <- randomValue :: RVar Laterality
    let lateralityG
            | (strictureG /= OCCLUSION) && (elem (active place1) [Specified TONGUEBLADE, Specified TONGUETIP, Specified TONGUEUNDER, Specified TONGUEBODY]) = Possible (Specified r2)
            | otherwise = Impossible

    r3 <- randomValue :: RVar Silibance
    let silibanceG
            | (strictureG == TURBULENT) && (lateralityG /= (Possible (Specified LATERAL))) && (trillG /= (Possible (Specified TRILLED))) = Possible (Specified r3)
            | otherwise = Impossible

    r4 <- choice [ZERO, NEGATIVE]
    r5 <- choice [ZERO, POSITIVE]
    let votG
            | (elem strictureG [OCCLUSION, TURBULENT]) && (trillG /= (Possible (Specified TRILLED))) && (phonationG == MODAL) = Possible (Specified r4)
            | (elem strictureG [OCCLUSION, TURBULENT]) && (trillG /= (Possible (Specified TRILLED))) && (phonationG /= MODAL) = Possible (Specified r5)
            | otherwise = Impossible

    let manner1 = Manner (Specified strictureG) trillG (Specified lengthG) lateralityG silibanceG (Specified airescapeG) votG

    let cfeat1 = ConsonantFeatures (Specified place1) (Specified manner1) (Specified airstream1) (Specified phonationG)

    return (ConsonantFeaturesContour cfeat1 Blank)

{-
generateVowel :: VowelFeaturesContour
generateVowel = VowelFeaturesContour (Filled (Specified vfeat1)) Blank

    let vfeat1 = VowelFeatures (Specified heightG)(Specified backnessG) (Specified roundednessG) (Specified lengthG) (Specified airescapeG)

    heightG <- Sampler.sample (randomValue :: RVar Height)
    backnessG <- Sampler.sample (randomValue :: RVar Backness)
    roundednessG <- Sampler.sample (randomValue :: RVar Roundedness)
    lengthG <- Sampler.sample (randomValue :: RVar Length)
    airescapeG <- Sampler.sample (randomValue :: RVar AirEscape)

    let airstream1 = Airstream (Specified initiatorG) (Specified directionG)
    initiatorG <- Sampler.sample (randomValue :: RVar Initiator)
    directionG <- Sampler.sample (randomValue :: RVar Direction)

    phonationG <- Sampler.sample (randomValue :: RVar Phonation)
-}
