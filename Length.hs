module Main where

import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = do
    bs <- BSL.getContents
    print $ BSL.length bs

