module HelperFunctions
( choice_
) where

import Data.RVar
import Data.Random.Extras

-- weighs the first argument by N
choice_ :: Int -> a -> a -> RVar a
choice_ wei fir sec = choice (sec : replicate wei fir)
