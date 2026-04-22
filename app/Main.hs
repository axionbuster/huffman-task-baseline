module Main (main) where

import qualified Data.ByteString as BS
import Huffman.Codec (decode, encode)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["encode"] ->
      BS.getContents >>= BS.putStr . encode
    ["decode"] -> do
      contents <- BS.getContents
      case decode contents of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right out -> BS.putStr out
    _ -> do
      hPutStrLn stderr "Usage: huffman encode|decode"
      exitFailure
