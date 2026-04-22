module Huffman.BitStream
  ( BitBuilder
  , emptyBitBuilder
  , putBit
  , putBits
  , putWord8Bits
  , finishBitBuilder
  , BitReader
  , newBitReader
  , getBit
  , getWord8Bits
  ) where

import Data.Bits (setBit, testBit)
import Data.ByteString.Builder (Builder, toLazyByteString, word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Word (Word8)

data BitBuilder = BitBuilder !Builder !Word8 !Int

emptyBitBuilder :: BitBuilder
emptyBitBuilder = BitBuilder mempty 0 0

putBit :: Bool -> BitBuilder -> BitBuilder
putBit bit (BitBuilder builder current usedBits)
  | usedBits == 7 =
      BitBuilder (builder <> word8 nextByte) 0 0
  | otherwise =
      BitBuilder builder nextByte (usedBits + 1)
  where
    nextByte
      | bit = setBit current (7 - usedBits)
      | otherwise = current

putBits :: [Bool] -> BitBuilder -> BitBuilder
putBits bits builder = foldl (flip putBit) builder bits

putWord8Bits :: Word8 -> BitBuilder -> BitBuilder
putWord8Bits value builder = foldl (\acc shift -> putBit (testBit value shift) acc) builder [7, 6 .. 0]

finishBitBuilder :: BitBuilder -> LBS.ByteString
finishBitBuilder (BitBuilder builder current usedBits)
  | usedBits == 0 = toLazyByteString builder
  | otherwise = toLazyByteString (builder <> word8 current)

data BitReader = BitReader
  { readerBytes :: !BS.ByteString
  , readerByteIndex :: !Int
  , readerBitIndex :: !Int
  }

newBitReader :: BS.ByteString -> Int -> BitReader
newBitReader bytes startByte = BitReader bytes startByte 0

getBit :: BitReader -> Maybe (Bool, BitReader)
getBit (BitReader bytes byteIndex bitIndex)
  | byteIndex >= BS.length bytes = Nothing
  | bitIndex == 7 =
      Just
        ( testBit currentByte 0
        , BitReader bytes (byteIndex + 1) 0
        )
  | otherwise =
      Just
        ( testBit currentByte (7 - bitIndex)
        , BitReader bytes byteIndex (bitIndex + 1)
        )
  where
    currentByte = BS.index bytes byteIndex

getWord8Bits :: BitReader -> Maybe (Word8, BitReader)
getWord8Bits reader = go 0 7 reader
  where
    go acc (-1) currentReader = Just (acc, currentReader)
    go acc shift currentReader = do
      (bit, nextReader) <- getBit currentReader
      let nextAcc
            | bit = setBit acc shift
            | otherwise = acc
      go nextAcc (shift - 1) nextReader
