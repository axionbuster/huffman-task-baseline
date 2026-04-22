module Huffman.Tree
  ( HTree (..)
  , buildTree
  , assignCodes
  ) where

import Data.Bits (shiftL, (.|.))
import Data.List (insertBy, sortBy)
import Data.Ord (comparing)
import Data.Word (Word8)

-- | Huffman tree node.
data HTree = Leaf !Word8 | Branch !HTree !HTree
  deriving (Show)

-- | Build a Huffman tree from a non-empty list of (byte, frequency) pairs.
-- Uses an ordered list as a simple priority queue (alphabet ≤ 256 so this is fine).
buildTree :: [(Word8, Int)] -> HTree
buildTree [] = error "buildTree: empty frequency list"
buildTree freqs = go initialQueue
  where
    initialQueue =
      sortBy (comparing fst) (map (\(b, f) -> (f, Leaf b)) freqs)

    go [(_, t)] = t
    go ((f1, t1) : (f2, t2) : rest) =
      go (insertBy (comparing fst) (f1 + f2, Branch t1 t2) rest)
    go [] = error "buildTree: impossible empty queue"

-- | Assign canonical prefix codes to every leaf.
-- Returns (byte, code, codeLen) triples.
-- Left branch appends a 0 bit; right branch appends a 1 bit (MSB-first).
-- For a single-leaf tree the code is (0, 1) — one zero bit per symbol.
assignCodes :: HTree -> [(Word8, Int, Int)]
assignCodes = go 0 0
  where
    go code len (Leaf b) = [(b, code, max 1 len)]
    go code len (Branch l r) =
      go (code `shiftL` 1) (len + 1) l
        ++ go ((code `shiftL` 1) .|. 1) (len + 1) r
