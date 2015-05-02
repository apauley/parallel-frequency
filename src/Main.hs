import System.Random

import Data.List

main :: IO ()
main = do
  seed <- newStdGen

  let numbers = take 50000 $ randomIntStream seed

  putStrLn $ summary $ frequency numbers

frequency :: Ord a => [a] -> [(a, Int)]
frequency xs = map (\l -> (head l, length l)) (group (sort xs))

summary :: [(Int, Int)] -> String
summary freq = "Frequency counts:\n" ++ counters ++ "\n" ++ total
  where counters = foldl countStr "" freq
        total    = "Total: " ++ show (foldl (+) 0 $ map snd freq) ++ "\n"

countStr :: String -> (Int, Int) -> String
countStr acc (a, count) = acc ++ show a ++ ":\t" ++ show count ++ "\n"

randomIntStream :: StdGen -> [Int]
randomIntStream = randomRs (1,5)
