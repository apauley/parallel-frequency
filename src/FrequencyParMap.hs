module FrequencyParMap where

import Data.List.Split (chunksOf)
import qualified Data.Map as Map

import Control.Parallel.Strategies hiding (parMap)
import Control.DeepSeq

import FrequencyMap (fromMap, frequencyMap)

type Count a = (Int, a)
type FrequencyCount a = [Count a]
type FrequencyMap a = Map.Map a Int

frequency :: (NFData a, Ord a) => [a] -> FrequencyCount a
frequency = fromMap . fold . frequencyMapMiniLists

frequencyMapMiniLists :: (NFData a, Ord a) => [a] -> [FrequencyMap a]
frequencyMapMiniLists as = runEval $ parMap frequencyMap (splitList as)

fold :: (NFData a, Ord a) => [FrequencyMap a] -> FrequencyMap a
fold = foldl (Map.unionWith (+)) Map.empty

splitList :: [a] -> [[a]]
splitList as = chunksOf 5000 as

parMap :: NFData b => (a -> b) -> [a] -> Eval [b]
parMap _ [] = return []
parMap f (a:as) = do
   b <- rpar $ force (f a)
   bs <- parMap f as
   return (b:bs)
