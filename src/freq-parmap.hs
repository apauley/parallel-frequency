import ChunkedByteStringMain (chunkedMain)
import FrequencyParMap (frequencyChunked)
-- import FrequencyParMap (frequency)

main :: IO ()
main = chunkedMain frequencyChunked
