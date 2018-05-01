module Main where

import qualified Data.ByteString.Streaming as BSS
import qualified Streaming.Prelude         as S

main :: IO ()
main = (S.sum $ BSS.unpack $ BSS.getContents) >>= print
