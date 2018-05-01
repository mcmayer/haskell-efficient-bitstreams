module Main where

import           Control.Monad.Identity    (Identity)
import           Data.Bits                 (shiftR, (.&.))
import qualified Data.ByteString.Streaming as BSS
import           Data.Word8                (Word8)
import qualified Streaming                 as S
import qualified Streaming.Internal        as S
import           Streaming.Prelude         (Of (..), Stream)
import qualified Streaming.Prelude         as S

{-
concat' :: (Foldable f, Monad m) => S.Stream (S.Of (f a)) m r ->  S.Stream (S.Of a) m r
concat' = loop
  where
    loop str = case str of
        S.Return r -> S.Return r
        S.Effect m -> S.Effect (fmap loop m)
        S.Step (lst :> as) ->
          let inner []       = loop as
              inner (x:rest) = S.Step (x :> inner rest)
          in inner (S.toList lst)
-}

splitByte :: Word8 -> [Bool]
splitByte w = (\i-> (w `shiftR` i) .&. 1 == 1) <$> [0..7]

bitStream :: Monad m => Stream (Of Word8) m () -> Stream (Of Bool) m ()
bitStream s = S.concat $ S.map splitByte s

main :: IO ()
main = do
    let bs = BSS.unpack BSS.getContents :: Stream (Of Word8) IO ()
        gs = S.group $ bitStream bs ::  Stream (Stream (Of Bool) IO) IO ()
    maxlen <- S.maximum $ S.mapped S.length gs
    print $ S.fst' maxlen

