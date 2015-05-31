import ChunkedMain (chunkedMain)
import FrequencyParMap (frequencyChunked)

main :: IO ()
main = chunkedMain frequencyChunked
