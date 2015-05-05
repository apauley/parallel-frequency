import System.Environment (getArgs)
import System.Random

import Shared
import Frequency1

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

randomIntStream :: StdGen -> [Int]
randomIntStream = randomRs (1,5)
