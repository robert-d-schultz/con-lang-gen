-- Data types for allophones (potentially anything though)
module RuleType
( 
PhonologicalRule(..)
, PhoneChange(..)
) where

import Prelude hiding (Word)
import Data.List
import PhoneType

data PhonologicalRule = PhonologicalRule 
    { change :: ConsonantPhoneChange
    , environment :: ConsonantPhoneEnvironment
    } deriving (Eq, Show, Read)

data PhoneChange = PhoneChange
    { from :: Either ConsonantFeaturesContour VowelFeaturesContour
    , to :: Either ConsonantFeaturesContour VowelFeaturesContour
    } deriving (Eq, Show, Read)

data PhoneEnvironment = PhoneEnvironment
    { beforeContext :: Maybe Context
    , beforeBoundary :: Maybe Boundary
    , afterContext :: Maybe Context
    , afterBoundary :: Maybe Boundary
    } deriving (Eq, Show, Read)

data Boundary =  NONE | SYLLABLE | MORPHEME | WORD deriving (Eq, Show, Read)
data Context = Context (Either Syllable (Either ConsonantFeaturesContour VowelFeaturesContour)) deriving (Eq, Show, Read)









