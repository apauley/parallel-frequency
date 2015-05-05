module Frequency1 where

import Shared
import Data.List

frequency :: Ord a => [a] -> FrequencyCount a
frequency as = reverse . sort $ map (\a -> (length a, head a)) (group (sort as))
