module Huffman.Codec
  ( decode,
    encode
  )
where

import Data.Array (assocs, (!))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder (byteString, toLazyByteString, word8)
import qualified Data.Map.Strict as Map
import Data.Word (Word16, Word64, Word8)
import Huffman.BitStream
  ( BitReader,
    bitReader,
    emptyBitWriter,
    finishBitWriter,
    putWord16BE,
    putWord64BE,
    readBit,
    readWord16BE,
    readWord64BE,
    writeBits
  )
import Huffman.Tree
  ( Codeword (..),
    DecodeTree (..),
    buildCodebook,
    buildDecodeTree
  )

magic :: B.ByteString
magic = B.pack [72, 85, 70, 49]

headerPrefixLength :: Int
headerPrefixLength = 14

encode :: B.ByteString -> B.ByteString
encode input =
  BL.toStrict . toLazyByteString $ header <> payload
  where
    frequencies = countFrequencies input
    codebook = buildCodebook frequencies
    usedSymbols = [(fromIntegral symbol, codeLength codeword) | (symbol, codeword) <- assocs codebook, codeLength codeword > 0]
    header =
      byteString magic
        <> putWord64BE (fromIntegral (B.length input))
        <> putWord16BE (fromIntegral (length usedSymbols) :: Word16)
        <> mconcat [word8 symbol <> word8 (fromIntegral codeLen) | (symbol, codeLen) <- usedSymbols]
    payload = finishBitWriter (B.foldl' writeSymbol emptyBitWriter input)
    writeSymbol writer byte =
      let Codeword codeValue' codeLength' = codebook ! fromIntegral byte
       in writeBits codeValue' codeLength' writer

decode :: B.ByteString -> Either String B.ByteString
decode input = do
  (originalLength, entries, payload) <- parseHeader input
  if originalLength == 0
    then Right B.empty
    else do
      tree <- buildDecodeTree entries
      decodePayload tree originalLength (bitReader payload)

parseHeader :: B.ByteString -> Either String (Word64, [(Word8, Int)], B.ByteString)
parseHeader input
  | B.length input < headerPrefixLength = Left "truncated header"
  | B.take 4 input /= magic = Left "invalid magic"
  | otherwise = do
      originalLength <- readWord64BE input 4
      symbolCount <- readWord16BE input 12
      let entryCount = fromIntegral symbolCount
          entriesOffset = headerPrefixLength
          payloadOffset = entriesOffset + (entryCount * 2)
      if B.length input < payloadOffset
        then Left "truncated header"
        else do
          entries <- mapM (readEntry input entriesOffset) [0 .. entryCount - 1]
          Right (originalLength, entries, B.drop payloadOffset input)

readEntry :: B.ByteString -> Int -> Int -> Either String (Word8, Int)
readEntry input baseOffset index =
  let offset = baseOffset + (index * 2)
   in if B.length input < offset + 2
        then Left "truncated header"
        else
          let symbol = B.index input offset
              len = fromIntegral (B.index input (offset + 1))
           in if len <= 0
                then Left "invalid code length"
                else Right (symbol, len)

countFrequencies :: B.ByteString -> Map.Map Int Word64
countFrequencies = B.foldl' step Map.empty
  where
    step counts byte = Map.insertWith (+) (fromIntegral byte) 1 counts

decodePayload :: DecodeTree -> Word64 -> BitReader -> Either String B.ByteString
decodePayload root originalLength reader =
  BL.toStrict . toLazyByteString <$> go originalLength reader root mempty
  where
    go 0 _ _ builder = Right builder
    go remaining currentReader (DecodeLeaf symbol) builder =
      go (remaining - 1) currentReader root (builder <> word8 symbol)
    go remaining currentReader (DecodeBranch left right) builder = do
      (bit, nextReader) <- readBit currentReader
      case if bit then right else left of
        Just nextTree -> go remaining nextReader nextTree builder
        Nothing -> Left "invalid bitstream"
