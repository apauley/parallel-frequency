module FrequencyParMap where

import Data.List.Split (chunksOf)
import qualified Data.Map as Map

import Control.Parallel.Strategies hiding (parMap)

import FrequencyMap (fromMap, frequencyMap)

type Count a = (Int, a)
type FrequencyCount a = [Count a]
type FrequencyMap a = Map.Map a Int

frequency :: Ord a => [a] -> FrequencyCount a
frequency = fromMap . fold . frequencyMapMiniLists

frequencyMapMiniLists :: Ord a => [a] -> [FrequencyMap a]
frequencyMapMiniLists as = runEval $ parMap frequencyMap (splitList as)

fold :: Ord a => [FrequencyMap a] -> FrequencyMap a
fold = foldl (Map.unionWith (+)) Map.empty

splitList :: [a] -> [[a]]
splitList as = chunksOf 50 as

parMap :: (a -> b) -> [a] -> Eval [b]
parMap _ [] = return []
parMap f (a:as) = do
   b <- rpar (f a)
   bs <- parMap f as
   return (b:bs)
