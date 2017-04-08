module HelperFunctions
( choice_
) where

import Data.RVar
import Data.Random.Extras

-- weighs the first argument by N
choice_ :: a -> a -> Int -> RVar a
choice_ fir sec wei = choice (sec : replicate wei fir)
