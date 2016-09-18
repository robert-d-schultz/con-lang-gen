{-# LANGUAGE ScopedTypeVariables #-}
module ConsonantPhoneGenAdvanced
( randomValue
, generateConsonant
) where

-- Import
import Prelude
import Data.RVar
import Data.Random hiding (sample)
import Data.Random.Extras hiding (shuffle)

-- Data structures
import PhonemeTypeAdvanced

-- Picks a random value of an Enum data type
randomValue :: forall t. (Bounded t, Enum t) => RVar t
randomValue = toEnum <$> (uniform (fromEnum (minBound :: t)) (fromEnum (maxBound :: t)))


-- Generates a completely random consonant
-- Ignores consonant contours, for the moment
generateConsonant :: RVar Consonant
generateConsonant = do
    let possiblePlaces = [Place LOWERLIP UPPERLIP
                        , Place LOWERLIP UPPERTEETH
                        , Place TONGUEBLADE UPPERLIP
                        , Place TONGUEBLADE UPPERTEETH
                        , Place TONGUEBLADE TEETHRIDGE
                        , Place TONGUEBLADE RIDGE
                        , Place TONGUEBLADE BACKRIDGE
                        , Place TONGUETIP UPPERTEETH
                        , Place TONGUETIP RIDGE
                        , Place TONGUETIP BACKRIDGE
                        , Place TONGUEUNDER HARDPALATE
                        , Place TONGUEBODY BACKRIDGE
                        , Place TONGUEBODY HARDPALATE
                        , Place TONGUEBODY SOFTPALATE
                        , Place TONGUEBODY UVULA
                        , Place TONGUEROOT PHARYNX
                        , Place LARYNX PHARYNX
                        , Place LARYNX EPIGLOTTIS
                        , Place LARYNX GLOTTIS
                        ]
    place1 <- choice possiblePlaces

    initiatorG <- randomValue :: RVar Initiator
    directionG <- randomValue :: RVar Direction
    let airstream1 = Airstream initiatorG directionG

    phonationG <- randomValue :: RVar Phonation

    strictureG <- randomValue :: RVar Stricture
    lengthG <- randomValue :: RVar Length
    airescapeG <- randomValue :: RVar AirEscape

    r1 <- randomValue :: RVar Trill
    let trillG
            | (elem (passive (place1)) [GLOTTIS, BACKRIDGE, HARDPALATE, SOFTPALATE]) = Impossible
            | otherwise = Possible (r1)


    r2 <- randomValue :: RVar Laterality
    let lateralityG
            | (strictureG /= OCCLUSION) && (elem (active place1) [TONGUEBLADE, TONGUETIP, TONGUEUNDER, TONGUEBODY]) = Possible (r2)
            | otherwise = Impossible

    r3 <- randomValue :: RVar Silibance
    let silibanceG
            | (strictureG == TURBULENT) && (lateralityG /= (Possible (LATERAL))) && (trillG /= (Possible (TRILLED))) = Possible (r3)
            | otherwise = Impossible

    r4 <- choice [ZERO, NEGATIVE]
    r5 <- choice [ZERO, POSITIVE]
    let votG
            | (elem strictureG [OCCLUSION, TURBULENT]) && (trillG /= (Possible (TRILLED))) && (phonationG == MODAL) = Possible (r4)
            | (elem strictureG [OCCLUSION, TURBULENT]) && (trillG /= (Possible (TRILLED))) && (phonationG /= MODAL) = Possible (r5)
            | otherwise = Impossible

    let manner1 = Manner strictureG trillG lengthG lateralityG silibanceG airescapeG votG

    return $ Consonant (NoContour place1) (NoContour manner1) (NoContour airstream1) (NoContour phonationG)

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
