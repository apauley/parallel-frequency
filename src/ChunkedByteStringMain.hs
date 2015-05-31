module ChunkedByteStringMain (chunkedMain) where

import System.Environment (getArgs)

import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Shared

chunkedMain :: (Show a, Ord a) => ([[T.Text]] -> FrequencyCount a) -> IO ()
chunkedMain frequency = do
  t0           <- printT0
  [fileName]   <- getArgs
  fileContents <- B.readFile fileName

  let strings = [fileContents]

  let source   = map T.decodeUtf8 strings
      charFreq = frequency (map T.words source)
  printTimeSince t0 "After freq return."

  putStrLn $ "\nTop 10 characters in " ++ fileName ++ ":"
  putStrLn $ summary (take 10 charFreq)
  printTimeSince t0 "After character frequency print."
