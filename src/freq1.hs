import System.Environment (getArgs)
import Data.Time (getCurrentTime)

import Shared

main :: IO ()
main = do
  [fileName] <- getArgs

  fileContents <- readFile fileName

  putStrLn "Calculate frequency of elements in a list."
  t0 <- getCurrentTime

  putStrLn $ "\nTop 10 words in " ++ fileName ++ ":"
  putStrLn $ summary $ take 10 $ frequency (words fileContents)
  printTimeSince t0 "after word frequency"

  putStrLn $ "\nTop 10 characters in " ++ fileName ++ ":"
  putStrLn $ summary $ take 10 $ frequency fileContents
  printTimeSince t0 "after char frequency"
