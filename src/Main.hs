import System.Environment (getArgs)

import System.Random

import Data.List

type Count a = (Int, a)
type FrequencyCount a = [Count a]

main :: IO ()
main = do
  [fileName] <- getArgs
  seed       <- newStdGen

  let numbers = take 1000000 $ randomIntStream seed
  putStrLn $ summary $ frequency numbers

  fileContents <- readFile fileName
  putStrLn $ summary $ frequency fileContents

frequency :: Ord a => [a] -> FrequencyCount a
frequency as = reverse . sort $ map (\a -> (length a, head a)) (group (sort as))

summary :: Show a => FrequencyCount a -> String
summary freq = "Frequency counts:\n" ++ counters ++ "\n" ++ total
  where counters = foldl countStr "" freq
        total    = "Total: " ++ show (foldl (+) 0 $ map fst freq) ++ "\n"

countStr :: Show a => String -> Count a -> String
countStr acc (count, a) = acc ++ show a ++ ":\t" ++ show count ++ "\n"

randomIntStream :: StdGen -> [Int]
randomIntStream = randomRs (1,5)
