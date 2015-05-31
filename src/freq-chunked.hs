import ChunkedMain (chunkedMain)
import FrequencyMapChunked (frequencyChunked)

main :: IO ()
main = chunkedMain frequencyChunked
