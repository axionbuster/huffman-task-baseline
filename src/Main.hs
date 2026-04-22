module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Huffman.Codec (decode, encode)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (stdin, stdout)

main :: IO ()
main = do
  args <- getArgs
  input <- BS.hGetContents stdin
  case args of
    ["encode"] -> LBS.hPut stdout (encode input)
    ["decode"] ->
      case decode input of
        Left message -> die message
        Right output -> LBS.hPut stdout output
    _ -> die "usage: huffman encode|decode"
