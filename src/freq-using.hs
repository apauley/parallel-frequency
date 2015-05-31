import ChunkedMain (chunkedMain)
import FrequencyMapUsing (frequencyChunked)

main :: IO ()
main = chunkedMain frequencyChunked
