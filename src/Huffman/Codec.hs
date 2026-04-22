module Huffman.Codec
  ( decode
  , encode
  ) where

import Data.Array (Array, array, (!))
import Data.Bits
import Data.ByteString.Builder (Builder, toLazyByteString, word8)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', unfoldr)
import Data.Word (Word64, Word8)
import Huffman.BitStream

data Tree
  = Leaf !Word8
  | Node !Tree !Tree

data WeightedTree = WeightedTree !Int !Word8 !Tree

encode :: BS.ByteString -> LBS.ByteString
encode input =
  header <> finishBitBuilder encodedBits
  where
    originalLength = fromIntegral (BS.length input) :: Word64
    header = magic <> encodeWord64LE originalLength
    encodedBits =
      case buildTree input of
        Nothing -> emptyBitBuilder
        Just tree ->
          let codes = buildCodes tree
              withTree = putTree tree emptyBitBuilder
           in BS.foldl' (\builder byte -> putBits (codes ! fromIntegral byte) builder) withTree input

decode :: BS.ByteString -> Either String LBS.ByteString
decode input
  | BS.length input < headerSize = Left "input too short"
  | BS.take 4 input /= magicBytes = Left "invalid magic header"
  | originalLength == 0 = Right LBS.empty
  | otherwise = do
      (tree, readerAfterTree) <- getTree (newBitReader input headerSize)
      if isSingleLeaf tree
        then pure (replicateLeaf originalLength tree)
        else decodePayload originalLength tree readerAfterTree
  where
    originalLength = decodeWord64LE (BS.take 8 (BS.drop 4 input))

magic :: LBS.ByteString
magic = LBS.fromStrict magicBytes

magicBytes :: BS.ByteString
magicBytes = BS.pack [72, 85, 70, 49]

headerSize :: Int
headerSize = 12

buildTree :: BS.ByteString -> Maybe Tree
buildTree =
  finish . foldl' step []
    . map toWeighted
    . IM.toAscList
    . BS.foldl' (\freqs byte -> IM.insertWith (+) (fromIntegral byte) 1 freqs) IM.empty
  where
    toWeighted (symbol, weight) = WeightedTree weight (fromIntegral symbol) (Leaf (fromIntegral symbol))
    finish [] = Nothing
    finish [WeightedTree _ _ tree] = Just tree
    finish (first : second : rest) = finish (insertWeighted (mergeTrees first second) rest)
    step acc item = insertWeighted item acc

mergeTrees :: WeightedTree -> WeightedTree -> WeightedTree
mergeTrees (WeightedTree leftWeight leftSymbol leftTree) (WeightedTree rightWeight rightSymbol rightTree) =
  WeightedTree
    (leftWeight + rightWeight)
    (min leftSymbol rightSymbol)
    (Node leftTree rightTree)

insertWeighted :: WeightedTree -> [WeightedTree] -> [WeightedTree]
insertWeighted item [] = [item]
insertWeighted item@(WeightedTree weight symbol _) (current@(WeightedTree currentWeight currentSymbol _) : rest)
  | (weight, symbol) <= (currentWeight, currentSymbol) = item : current : rest
  | otherwise = current : insertWeighted item rest

buildCodes :: Tree -> Array Int [Bool]
buildCodes tree =
  array (0, 255) ([ (index, []) | index <- [0 .. 255] ] <> go [] tree)
  where
    go path (Leaf symbol) = [(fromIntegral symbol, reverse path)]
    go path (Node left right) = go (False : path) left <> go (True : path) right

putTree :: Tree -> BitBuilder -> BitBuilder
putTree (Leaf symbol) builder = putWord8Bits symbol (putBit True builder)
putTree (Node left right) builder = putTree right (putTree left (putBit False builder))

getTree :: BitReader -> Either String (Tree, BitReader)
getTree reader =
  case getBit reader of
    Nothing -> Left "unexpected end of input while reading tree"
    Just (isLeaf, nextReader)
      | isLeaf ->
          case getWord8Bits nextReader of
            Nothing -> Left "unexpected end of input while reading tree leaf"
            Just (symbol, afterSymbol) -> Right (Leaf symbol, afterSymbol)
      | otherwise -> do
          (leftTree, afterLeft) <- getTree nextReader
          (rightTree, afterRight) <- getTree afterLeft
          Right (Node leftTree rightTree, afterRight)

decodePayload :: Word64 -> Tree -> BitReader -> Either String LBS.ByteString
decodePayload remaining tree reader = fmap toLazyByteString (go remaining tree reader mempty)
  where
    go 0 _ _ acc = Right acc
    go count (Leaf symbol) currentReader acc = go (count - 1) tree currentReader (acc <> word8 symbol)
    go count (Node left right) currentReader acc =
      case getBit currentReader of
        Nothing -> Left "unexpected end of input while decoding payload"
        Just (bit, nextReader) ->
          go count (if bit then right else left) nextReader acc

isSingleLeaf :: Tree -> Bool
isSingleLeaf Leaf {} = True
isSingleLeaf Node {} = False

replicateLeaf :: Word64 -> Tree -> LBS.ByteString
replicateLeaf count (Leaf symbol) = LBS.fromChunks (go count)
  where
    chunkSize = 32768
    go 0 = []
    go remaining =
      let currentChunk = fromIntegral (min remaining (fromIntegral chunkSize))
       in BS.replicate currentChunk symbol : go (remaining - fromIntegral currentChunk)
replicateLeaf _ Node {} = LBS.empty

encodeWord64LE :: Word64 -> LBS.ByteString
encodeWord64LE value = LBS.pack (unfoldr nextByte (value, 0 :: Int))
  where
    nextByte (_, 8) = Nothing
    nextByte (currentValue, offset) =
      Just (fromIntegral (currentValue `shiftR` (offset * 8)), (currentValue, offset + 1))

decodeWord64LE :: BS.ByteString -> Word64
decodeWord64LE bytes =
  foldl'
    (\acc (shift, byte) -> acc .|. (fromIntegral byte `shiftL` shift))
    0
    (zip [0, 8 ..] (BS.unpack bytes))
