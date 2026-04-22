module Huffman.Tree
  ( Codeword (..),
    DecodeTree (..),
    buildCodebook,
    buildDecodeTree
  )
where

import Data.Array (Array, array)
import Data.Bits (shiftL, testBit)
import Data.List (insertBy, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64, Word8)

data HuffmanTree
  = HuffLeaf !Int
  | HuffBranch !HuffmanTree !HuffmanTree

newtype QueueNode = QueueNode (Word64, Int, HuffmanTree)

instance Eq QueueNode where
  QueueNode (weightA, orderA, _) == QueueNode (weightB, orderB, _) =
    weightA == weightB && orderA == orderB

instance Ord QueueNode where
  compare (QueueNode (weightA, orderA, _)) (QueueNode (weightB, orderB, _)) =
    compare (weightA, orderA) (weightB, orderB)

queueTree :: QueueNode -> HuffmanTree
queueTree (QueueNode (_, _, tree)) = tree

queueWeight :: QueueNode -> Word64
queueWeight (QueueNode (weight, _, _)) = weight

data Codeword = Codeword
  { codeValue :: !Integer,
    codeLength :: !Int
  }

absentCodeword :: Codeword
absentCodeword = Codeword 0 0

data DecodeTree
  = DecodeLeaf !Word8
  | DecodeBranch !(Maybe DecodeTree) !(Maybe DecodeTree)

buildCodebook :: Map Int Word64 -> Array Int Codeword
buildCodebook frequencies =
  array (0, 255) [(symbol, Map.findWithDefault absentCodeword symbol codebook) | symbol <- [0 .. 255]]
  where
    codebook = Map.fromList (canonicalAssignments (buildCodeLengths frequencies))

buildDecodeTree :: [(Word8, Int)] -> Either String DecodeTree
buildDecodeTree entries = do
  root <- foldl insertEntry (Right Nothing) (canonicalAssignments lengths)
  case root of
    Just tree -> Right tree
    Nothing -> Left "empty Huffman tree"
  where
    lengths = [(fromIntegral symbol, len) | (symbol, len) <- entries, len > 0]
    insertEntry acc (symbol, codeword) = do
      tree <- acc
      insertCode tree (fromIntegral symbol) codeword

canonicalAssignments :: [(Int, Int)] -> [(Int, Codeword)]
canonicalAssignments lengths = reverse assignments
  where
    sorted = sortOn (\(symbol, len) -> (len, symbol)) (filter ((> 0) . snd) lengths)
    (_, _, assignments) = foldl assign (0 :: Integer, 0 :: Int, []) sorted
    assign (nextCode, previousLength, acc) (symbol, len) =
      let aligned = if previousLength == 0 then 0 else nextCode `shiftL` (len - previousLength)
       in (aligned + 1, len, (symbol, Codeword aligned len) : acc)

insertCode :: Maybe DecodeTree -> Word8 -> Codeword -> Either String (Maybe DecodeTree)
insertCode root symbol codeword = go root (codeLength codeword)
  where
    code = codeValue codeword

    go current 0 =
      case current of
        Nothing -> Right (Just (DecodeLeaf symbol))
        _ -> Left "invalid code table"
    go current remaining =
      let nextBit = testBit code (remaining - 1)
       in case current of
            Nothing ->
              if nextBit
                then do
                  child <- go Nothing (remaining - 1)
                  Right (Just (DecodeBranch Nothing child))
                else do
                  child <- go Nothing (remaining - 1)
                  Right (Just (DecodeBranch child Nothing))
            Just (DecodeBranch left right) ->
              if nextBit
                then do
                  child <- go right (remaining - 1)
                  Right (Just (DecodeBranch left child))
                else do
                  child <- go left (remaining - 1)
                  Right (Just (DecodeBranch child right))
            Just (DecodeLeaf _) -> Left "invalid code table"

buildCodeLengths :: Map Int Word64 -> [(Int, Int)]
buildCodeLengths frequencies =
  case initialQueue of
    [] -> []
    [QueueNode (_, _, HuffLeaf symbol)] -> [(candidate, if candidate == symbol then 1 else 0) | candidate <- [0 .. 255]]
    queue ->
      let root = buildTree 256 queue
          lengths = collectLengths 0 root
       in [(candidate, Map.findWithDefault 0 candidate lengths) | candidate <- [0 .. 255]]
  where
    initialQueue =
      foldr
        (insertBy compare)
        []
        [QueueNode (weight, symbol, HuffLeaf symbol) | (symbol, weight) <- Map.toAscList frequencies, weight > 0]

    buildTree _ [node] = queueTree node
    buildTree nextOrder (first : second : rest) =
      let combined =
            QueueNode
              ( queueWeight first + queueWeight second,
                nextOrder,
                HuffBranch (queueTree first) (queueTree second)
              )
       in buildTree (nextOrder + 1) (insertBy compare combined rest)
    buildTree _ [] = error "impossible"

collectLengths :: Int -> HuffmanTree -> Map Int Int
collectLengths depth (HuffLeaf symbol) = Map.singleton symbol depth
collectLengths depth (HuffBranch left right) =
  Map.union (collectLengths (depth + 1) left) (collectLengths (depth + 1) right)
