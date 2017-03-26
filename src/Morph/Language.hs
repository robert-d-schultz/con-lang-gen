module Morph.Language
( morphLanguage
) where

import Data.RVar

import Data.Other

-- identity
morphLanguage :: Language -> RVar Language
morphLanguage parent = return parent
