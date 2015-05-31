{-# LANGUAGE BangPatterns #-}

-- The code in this file was crafted by Claude Heiland-Allen
-- For more details see: http://code.mathr.co.uk/word-histogram

-- | Split a UTF-8 ByteString into chunks.
module Utf8Chunk (utf8Chunk) where

import Data.Bits ((.&.))
import Data.Word (Word8)
import qualified Data.ByteString    as B        -- bytestring
import qualified Data.Text          as T        -- text
import qualified Data.Text.Encoding as T        -- text

-- | Split a strict ByteString containing UTF-8 into chunks, each no smaller
--   than the desired size.
utf8Chunk
  :: (Char -> Bool)   -- ^ which characters are allowable break points
  -> Int              -- ^ desired chunk size in bytes
  -> B.ByteString     -- ^ must be valid UTF-8
  -> [B.ByteString]   -- ^ valid UTF-8 chunks split at allowable points
utf8Chunk canBreak approxChunkBytes = go
  where
    go utf8Input
      | B.null utf8Input = []
      | otherwise =
          let -- skip the target number of bytes
              post = B.drop approxChunkBytes utf8Input
              -- ensure multi-byte characters are synchronized
              (cont, rest) = B.span isUtf8ContinuationByte post
              -- ensure it is safe to break (eg: not in hte middle of a word)
              block = breakLengthUtf8 canBreak rest
              -- find the true chunk length taking the above into account
              chunkBytes = approxChunkBytes + B.length cont + block
              -- split the input without copying
              (prefix, suffix) = B.splitAt chunkBytes utf8Input
              -- decode the chunk and continue with the remainder
          in  prefix : go suffix

isUtf8ContinuationByte :: Word8 -> Bool
-- https://en.wikipedia.org/wiki/UTF-8#Description
isUtf8ContinuationByte w = w .&. 0xC0 == 0x80

breakLengthUtf8 :: (Char -> Bool) -> B.ByteString -> Int
-- keep getting text until it's safe to break
-- return the number of bytes consumed
breakLengthUtf8 canBreak = go 0
  where
    go !count s = case fromUtf8 s of
      Nothing -> count
      Just (t, bytes)
        | canBreak (T.last t) -> count
        | otherwise -> go (count + bytes) (B.drop bytes s)

fromUtf8 :: B.ByteString -> Maybe (T.Text, Int)
-- feed in a byte at a time until some text can be decoded or there are no more
fromUtf8
  = go 0 (T.Some T.empty B.empty T.streamDecodeUtf8)
  . map B.singleton . B.unpack
  where
    go !count (T.Some t _ next) bs
      | T.null t = case bs of
          [] -> Nothing
          (b:rest) -> go (count + 1) (next b) rest
      | otherwise = Just (t, count)
