module FrequencyMapParUnion where

import qualified Data.Map as Map

import Control.Parallel.Strategies hiding (parMap)
import Control.DeepSeq

import FrequencyMap (FrequencyMap, fromMap, frequencyMap)
import Shared (FrequencyCount, split)

frequency :: (NFData a, Ord a) => [a] -> FrequencyCount a
frequency = fromMap . runEval . fold . frequencyMapChunkedlist

frequencyMapChunkedlist :: (NFData a, Ord a) => [a] -> [FrequencyMap a]
frequencyMapChunkedlist as = map frequencyMap (split as) `using` parList (rpar . force)

fold :: (NFData a, Ord a) => [FrequencyMap a] -> Eval (FrequencyMap a)
fold = foldl parUnion (rseq Map.empty)

parUnion :: (NFData a, Ord a) => Eval (FrequencyMap a) -> FrequencyMap a -> Eval (FrequencyMap a)
parUnion m1 m2 = do
  m1' <- m1
  u   <- rpar (Map.unionWith (+) m1' m2)
  return u
