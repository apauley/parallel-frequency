module FrequencyParMap where

import qualified Data.Map as Map

import Control.Parallel.Strategies hiding (parMap)
import Control.DeepSeq

import FrequencyMap (fromMap, frequencyMap)
import Shared (split)

type Count a = (a, Int)
type FrequencyCount a = [Count a]
type FrequencyMap a = Map.Map a Int

frequency :: (NFData a, Ord a) => [a] -> FrequencyCount a
frequency = fromMap . fold . frequencyMapChunkedlist

frequencyMapChunkedlist :: (NFData a, Ord a) => [a] -> [FrequencyMap a]
frequencyMapChunkedlist as = runEval $ parMap frequencyMap (split as)

fold :: (NFData a, Ord a) => [FrequencyMap a] -> FrequencyMap a
fold = foldl (Map.unionWith (+)) Map.empty

parMap :: NFData b => (a -> b) -> [a] -> Eval [b]
parMap _ [] = return []
parMap f (a:as) = do
   b <- rpar $ force (f a)
   bs <- parMap f as
   return (b:bs)
