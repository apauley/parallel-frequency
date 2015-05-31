import ChunkedByteStringMain (chunkedMain)
import FrequencyMapChunkedList (frequencyChunked)

main :: IO ()
main = chunkedMain frequencyChunked
