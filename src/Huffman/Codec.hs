module Huffman.Codec
  ( encode
  , decode
  ) where

import Data.Array (Array, listArray, (!))
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64, Word8)

import Huffman.Tree (HTree (..), assignCodes, buildTree)

-- ---------------------------------------------------------------------------
-- Wire format
--   4 bytes  : magic 0x48 0x55 0x46 0x46  ("HUFF")
--   8 bytes  : original byte count, Word64 little-endian
--   1024 bytes: frequency table, 256 × Word32 little-endian
--   n bytes  : Huffman-coded bits, MSB-first, zero-padded to byte boundary
-- ---------------------------------------------------------------------------

-- | Count byte frequencies.
countFreqs :: ByteString -> [(Word8, Int)]
countFreqs =
  Map.toList . BS.foldl' (\m b -> Map.insertWith (+) b 1 m) Map.empty

-- | Build code-lookup array indexed by byte value.
buildCodeTable :: [(Word8, Int, Int)] -> Array Word8 (Int, Int)
buildCodeTable triples =
  listArray (minBound, maxBound) (map look [minBound .. maxBound])
  where
    m = Map.fromList [(b, (c, l)) | (b, c, l) <- triples]
    look b = Map.findWithDefault (0, 0) b m

-- ---------------------------------------------------------------------------
-- Encoder
-- ---------------------------------------------------------------------------

encode :: ByteString -> ByteString
encode bs = BL.toStrict . BB.toLazyByteString $ hdr <> bdy
  where
    origLen = fromIntegral (BS.length bs) :: Word64
    freqs = countFreqs bs
    freqMap = Map.fromList freqs

    -- 256-entry frequency table, ordered by byte value
    freqArr =
      [ fromIntegral (Map.findWithDefault 0 b freqMap) :: Word32
      | b <- [minBound .. maxBound]
      ]

    hdr =
      BB.word8 0x48
        <> BB.word8 0x55
        <> BB.word8 0x46
        <> BB.word8 0x46
        <> BB.word64LE origLen
        <> mconcat (map BB.word32LE freqArr)

    bdy
      | BS.null bs = mempty
      | otherwise = packBits (buildCodeTable (assignCodes (buildTree freqs))) bs

-- | Pack bytes into a bit stream (MSB-first) using a Word64 shift register.
packBits :: Array Word8 (Int, Int) -> ByteString -> BB.Builder
packBits table bs = finalise (BS.foldl' step (0 :: Word64, 0 :: Int, mempty) bs)
  where
    step (buf, cnt, acc) b =
      let (code, len) = table ! b
          buf' = buf .|. (fromIntegral code `shiftL` (64 - cnt - len))
          cnt' = cnt + len
      in flushBytes buf' cnt' acc

    flushBytes buf cnt acc
      | cnt >= 8 =
          flushBytes
            (buf `shiftL` 8)
            (cnt - 8)
            (acc <> BB.word8 (fromIntegral (buf `shiftR` 56)))
      | otherwise = (buf, cnt, acc)

    finalise (buf, cnt, acc)
      | cnt > 0 = acc <> BB.word8 (fromIntegral (buf `shiftR` 56))
      | otherwise = acc

-- ---------------------------------------------------------------------------
-- Decoder
-- ---------------------------------------------------------------------------

decode :: ByteString -> Either String ByteString
decode bs = do
  -- Magic
  let (mag, r0) = BS.splitAt 4 bs
  if BS.unpack mag /= [0x48, 0x55, 0x46, 0x46]
    then Left "huffman: invalid magic"
    else Right ()

  -- Original length
  let (lenBytes, r1) = BS.splitAt 8 r0
  if BS.length lenBytes < 8
    then Left "huffman: truncated header (origLen)"
    else Right ()
  let origLen = readWord64LE lenBytes

  -- Frequency table
  let (freqBytes, body) = BS.splitAt 1024 r1
  if BS.length freqBytes < 1024
    then Left "huffman: truncated header (freq table)"
    else Right ()
  let freqs = readFreqTable freqBytes

  if origLen == 0
    then Right BS.empty
    else do
      let nonZero =
            [ (b, fromIntegral f)
            | (b, f) <- zip [minBound ..] freqs
            , f > (0 :: Word32)
            ]
      if null nonZero
        then Left "huffman: no frequencies but origLen > 0"
        else Right ()
      Right (unpackBits (buildTree nonZero) (fromIntegral origLen) body)

-- | Read a little-endian Word64 from the first 8 bytes of a ByteString.
readWord64LE :: ByteString -> Word64
readWord64LE bs =
  foldl
    (\acc (i, b) -> acc .|. (fromIntegral b `shiftL` (8 * i)))
    0
    (zip [0 ..] (BS.unpack (BS.take 8 bs)))

-- | Read 256 little-endian Word32 values from a 1024-byte ByteString.
readFreqTable :: ByteString -> [Word32]
readFreqTable bs = go (BS.unpack bs)
  where
    go (b0 : b1 : b2 : b3 : rest) =
      ( fromIntegral b0
          .|. (fromIntegral b1 `shiftL` 8)
          .|. (fromIntegral b2 `shiftL` 16)
          .|. (fromIntegral b3 `shiftL` 24)
      )
        : go rest
    go _ = []

-- | Walk the Huffman tree bit-by-bit (MSB-first) and emit decoded bytes.
unpackBits :: HTree -> Int -> ByteString -> ByteString
unpackBits root origLen body =
  BL.toStrict . BB.toLazyByteString $
    goBytes root origLen (BS.unpack body)
  where
    -- Process one byte of encoded data at a time.
    goBytes _ 0 _ = mempty
    goBytes _ _ [] = mempty -- ran out of encoded bits
    goBytes node n (byte : rest) = goBits node n byte 7 rest

    -- Process one bit (bit position i, MSB = 7, LSB = 0).
    goBits _ 0 _ _ _ = mempty
    goBits node n byte i rest =
      let bit = (byte `shiftR` i) .&. (1 :: Word8)
      in case node of
        -- Root is a single-leaf tree: every bit represents one symbol.
        Leaf b ->
          BB.word8 b
            <> advance root (n - 1) byte i rest
        Branch l r ->
          let child = if bit == 0 then l else r
          in case child of
            Leaf b ->
              BB.word8 b
                <> advance root (n - 1) byte i rest
            _ ->
              advance child n byte i rest

    -- Move to the next bit position (or next byte).
    advance _ 0 _ _ _ = mempty
    advance node n byte i rest =
      if i == 0
        then goBytes node n rest
        else goBits node n byte (i - 1) rest
