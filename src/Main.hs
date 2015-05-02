import System.Environment (getArgs)

import System.Random

import Data.List

type Count a = (Int, a)
type FrequencyCount a = [Count a]

main :: IO ()
main = do
  [fileName] <- getArgs
  seed       <- newStdGen

  fileContents <- readFile fileName

  putStrLn $ "Top 10 words in " ++ fileName ++ ":"
  putStrLn $ summary $ take 10 $ frequency (words fileContents)

  putStrLn $ "Top 10 characters in " ++ fileName ++ ":"
  putStrLn $ summary $ take 10 $ frequency fileContents

  let numbers = take 1000000 $ randomIntStream seed
  putStrLn $ "Frequency count for " ++ show (length numbers) ++ " random numbers:"
  putStrLn $ summary $ frequency numbers

frequency :: Ord a => [a] -> FrequencyCount a
frequency as = reverse . sort $ map (\a -> (length a, head a)) (group (sort as))

summary :: Show a => FrequencyCount a -> String
summary freq = counters ++ "\n" ++ total
  where counters = foldl countStr "" freq
        total    = "Total: " ++ show (foldl (+) 0 $ map fst freq) ++ "\n"

countStr :: Show a => String -> Count a -> String
countStr acc (count, a) = acc ++ show a ++ ":\t" ++ show count ++ "\n"

randomIntStream :: StdGen -> [Int]
randomIntStream = randomRs (1,5)
