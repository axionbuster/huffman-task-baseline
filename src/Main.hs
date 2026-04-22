module Main (main) where

import qualified Data.ByteString as B
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (stdin, stdout)
import Huffman.Codec (decode, encode)

main :: IO ()
main = do
  args <- getArgs
  input <- B.hGetContents stdin
  case args of
    ["encode"] -> B.hPut stdout (encode input)
    ["decode"] ->
      case decode input of
        Left err -> die err
        Right output -> B.hPut stdout output
    _ -> die "usage: huffman encode|decode"
