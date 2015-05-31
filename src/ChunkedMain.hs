module ChunkedMain (chunkedMain) where

import System.Environment (getArgs)
import GHC.Conc (getNumCapabilities)

import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Utf8Chunk (utf8Chunk)
import Shared

chunkedMain :: (Show a, Ord a) => ([[T.Text]] -> FrequencyCount a) -> IO ()
chunkedMain frequency = do
  t0           <- printT0
  cores        <- getNumCapabilities
  [fileName]   <- getArgs
  fileContents <- B.readFile fileName

  let fileLength = B.length fileContents                     -- O(1)
      chunkSize  = div fileLength cores                      -- O(1)
      canBreak   = const True
      strings    = utf8Chunk canBreak chunkSize fileContents -- O(cores)
      counter    = map T.singleton . T.unpack
      source     = map T.decodeUtf8 strings
      charFreq   = frequency (map counter source)
  printTimeSince t0 "After freq return."

  putStrLn $ "\nTop 10 characters in " ++ fileName ++ ":"
  putStrLn $ summary (take 10 charFreq)
  printTimeSince t0 "After character frequency print."
