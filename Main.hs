module Main where

import           Control.Monad                   (forM, forM_)
import           Control.Monad.State
import           Control.Monad.Trans.Resource    (runResourceT)
import           Data.Bits                       (shiftR, (.&.))
import qualified Data.ByteString.Lazy            as BSL
import qualified Data.ByteString.Streaming       as Q
import qualified Data.ByteString.Streaming.Char8 as Q
import           Data.Char                       (toUpper)
import           Data.Int                        (Int64)
import           Data.Word8                      (Word8)
import           Streaming                       (Of, Stream)
import qualified Streaming                       as S
import qualified Streaming.Prelude               as S
import           System.IO                       (IOMode (..), withFile)

head' :: Int -> FilePath -> IO ()
head' n file = withFile file ReadMode $ \h ->
    Q.stdout         -- IO ()                      -- stream to IO.stdout
    $ Q.unlines      -- ByteString m ()            -- insert '\n' between bytestream layers
    $ S.take n        -- Stream (ByteString m) m () -- nb. "takes" is 'functor general'
    $ Q.lines        -- Stream (ByteString m) m () -- divided into Stream layers
    $ Q.fromHandle h -- ByteString m ()            -- raw bytes

type MyState = State (Int64, BSL.ByteString)

s :: State Word8 String
s = do
    --put 5
    modify (1+)
    show <$> get

bs :: Word8 -> Stream (Of Bool) MyState ()
bs  byte= do
    forM_ [0..7] $ \i->
        S.yield $ (byte `shiftR` i) .&. 1 == 1

splitByte :: Word8 -> [Bool]
splitByte w = Prelude.map (\i-> (w `shiftR` i) .&. 1 == 1) [0..7]

bitStream :: BSL.ByteString -> [Bool]
bitStream bs = concat $ map splitByte (BSL.unpack bs)

main :: IO ()
main = do
    bs <- BSL.getContents
    print $ sum $ map (\b->if b then 1::Word8 else 0::Word8) $ bitStream bs



{-
    let result = runState s 0
    print result
    let bytes = [1,2,3,255::Word8]
    let byteStream = S.each bytes :: Stream (Of Word8) IO ()
    let byteStream' = S.each bytes :: Stream (Of Word8) MyState ()
    print bytes
    let xyz = evalState $ S.toList byteStream'
    dat' <- BSL.readFile "data.in"
    --let dat = Q.fromLazy dat'
    let streamIn = S.yield "asdf"
    S.stdoutLn $ S.map (map toUpper) streamIn
    S.stdoutLn $ S.map show byteStream
    --S.stdoutLn $ S.map show byteStream'
    head' 10 "text.in"
-}
