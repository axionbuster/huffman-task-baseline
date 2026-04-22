module Huffman.BitStream
  ( BitReader,
    BitWriter,
    bitReader,
    emptyBitWriter,
    finishBitWriter,
    putWord16BE,
    putWord64BE,
    readBit,
    readWord16BE,
    readWord64BE,
    writeBit,
    writeBits
  )
where

import Data.Bits ((.|.), shiftL, shiftR, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder, word8)
import Data.Word (Word16, Word64, Word8)

data BitWriter = BitWriter !Builder !Word8 !Int

data BitReader = BitReader !ByteString !Int !Int

bitReader :: ByteString -> BitReader
bitReader bytes = BitReader bytes 0 0

emptyBitWriter :: BitWriter
emptyBitWriter = BitWriter mempty 0 0

writeBit :: Bool -> BitWriter -> BitWriter
writeBit bit (BitWriter builder current filled) =
  let current' = (current `shiftL` 1) .|. if bit then 1 else 0
      filled' = filled + 1
   in if filled' == 8
        then BitWriter (builder <> word8 current') 0 0
        else BitWriter builder current' filled'

writeBits :: Integer -> Int -> BitWriter -> BitWriter
writeBits _ 0 writer = writer
writeBits code bitCount writer = go (bitCount - 1) writer
  where
    go (-1) acc = acc
    go index acc = go (index - 1) (writeBit (testBit code index) acc)

finishBitWriter :: BitWriter -> Builder
finishBitWriter (BitWriter builder current filled)
  | filled == 0 = builder
  | otherwise = builder <> word8 (current `shiftL` (8 - filled))

readBit :: BitReader -> Either String (Bool, BitReader)
readBit (BitReader bytes byteIndex bitIndex)
  | byteIndex >= B.length bytes = Left "unexpected end of input"
  | otherwise =
      let current = B.index bytes byteIndex
          bit = testBit current (7 - bitIndex)
          nextBitIndex = bitIndex + 1
       in if nextBitIndex == 8
            then Right (bit, BitReader bytes (byteIndex + 1) 0)
            else Right (bit, BitReader bytes byteIndex nextBitIndex)

putWord16BE :: Word16 -> Builder
putWord16BE value =
  word8 (fromIntegral (value `shiftR` 8))
    <> word8 (fromIntegral value)

putWord64BE :: Word64 -> Builder
putWord64BE value =
  mconcat
    [ word8 (fromIntegral (value `shiftR` 56)),
      word8 (fromIntegral (value `shiftR` 48)),
      word8 (fromIntegral (value `shiftR` 40)),
      word8 (fromIntegral (value `shiftR` 32)),
      word8 (fromIntegral (value `shiftR` 24)),
      word8 (fromIntegral (value `shiftR` 16)),
      word8 (fromIntegral (value `shiftR` 8)),
      word8 (fromIntegral value)
    ]

readWord16BE :: ByteString -> Int -> Either String Word16
readWord16BE bytes offset
  | B.length bytes < offset + 2 = Left "truncated header"
  | otherwise =
      let b0 = fromIntegral (B.index bytes offset) :: Word16
          b1 = fromIntegral (B.index bytes (offset + 1)) :: Word16
       in Right ((b0 `shiftL` 8) .|. b1)

readWord64BE :: ByteString -> Int -> Either String Word64
readWord64BE bytes offset
  | B.length bytes < offset + 8 = Left "truncated header"
  | otherwise =
      let step acc index = (acc `shiftL` 8) .|. fromIntegral (B.index bytes (offset + index))
       in Right (foldl step 0 [0 .. 7])
