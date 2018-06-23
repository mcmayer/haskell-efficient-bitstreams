{-# LANGUAGE RankNTypes #-}
module Main where

{- this is Cirdec's solution https://github.com/Cedev/haskell-efficient-bitstreams/blob/master/LongestSeqFuse.hs -}

import           Data.Bits            (shiftR, testBit, (.&.))
import qualified Data.ByteString.Lazy as BSL
import           Data.List            (foldl')
import           Data.Word8           (Word8)

-- A list encoded as a strict left fold.
newtype ListS a = ListS {build :: forall b. (b -> a -> b) -> b -> b}

instance Functor ListS where
  fmap = smap

{-# INLINE smap #-}
-- fmap renamed so it can be inlined
smap f l = ListS (\c z -> build l (\z a -> c z (f a)) z)


{-# INLINE fromList #-}
fromList :: [a] -> ListS a
fromList l = ListS (\c z -> foldl' c z l)

{-# INLINE fromBS #-}
fromBS :: BSL.ByteString -> ListS Word8
fromBS l = ListS (\c z -> BSL.foldl' c z l)

{-# INLINE length' #-}
length' :: ListS a -> Int
length' l = build l (\z a -> z+1) 0

{-# INLINE splitByte #-}
splitByte :: Word8 -> [Bool]
splitByte w = Prelude.map (\i-> (w `shiftR` i) .&. 1 == 1) [0..7]

{-# INLINE splitByte' #-}
splitByte' :: Word8 -> ListS Bool
splitByte' = fromList . splitByte

splitByte'' :: Word8 -> ListS Bool
splitByte'' w = ListS (\c z -> z `c` testBit w 0 `c` testBit w 1 `c` testBit w 2 `c` testBit w 3 `c` testBit w 4 `c` testBit w 5 `c` testBit w 6 `c` testBit w 7)

{-# INLINE concat' #-}
concat' :: ListS (ListS a) -> ListS a
concat' ll = ListS (\c z -> build ll (\z l -> build l c z) z)

{-# INLINE bitStream' #-}
bitStream' :: BSL.ByteString -> ListS Bool
bitStream' = concat' . smap splitByte' . fromBS

{-# INLINE bits #-}
-- A hand-unrolled version of `concat' . smap splitByte'`
bits :: ListS Word8 -> ListS Bool
bits l = ListS (\c z -> build l (\z w -> z `c` testBit w 0 `c` testBit w 1 `c` testBit w 2 `c` testBit w 3 `c` testBit w 4 `c` testBit w 5 `c` testBit w 6 `c` testBit w 7) z)

{-# INLINE bitStream'' #-}
-- not as fast as bitStream' !
bitStream'' :: BSL.ByteString -> ListS Bool
bitStream'' = bits . fromBS

{-# INLINE scanlStep #-}
scanlStep :: (b -> a -> b) -> (c -> b -> c) -> (b, c) -> a -> (b, c)
scanlStep f c = \(y, z) x -> (y `f` x, z `c` y)

{-# INLINE uncurryScanl #-}
uncurryScanl :: (c -> b -> c) -> (b, c) -> c
uncurryScanl f (b, c) = f c b

{-# INLINE scanl' #-}
scanl' :: (b -> a -> b) -> b -> ListS a -> ListS b
scanl' f y l = ListS (\c z -> uncurryScanl c (build l (scanlStep f c) (y, z)))

{-# INLINE filter' #-}
filter' :: (a -> Bool) -> ListS a -> ListS a
filter' f l = ListS (\c z -> build l (\acc x -> if f x then acc `c` x else acc) z)

data LongestRun = LongestRun !Bool !Int !Int

{-# INLINE extendRun #-}
extendRun (LongestRun previous run longest) x  = LongestRun x current (max current longest)
  where
    current = if x == previous then run + 1 else 1

{-# INLINE longestRun #-}
longestRun :: ListS Bool -> Int
longestRun l = longest
 where
   (LongestRun _ _ longest) = build l extendRun (LongestRun False 0 0)


{-# INLINE longestRun' #-}
longestRun' :: BSL.ByteString -> Int
-- A hand-combined version of everything
longestRun' l = longest
 where
   (LongestRun _ _ longest) = BSL.foldl' (\z w -> z `extendRun` testBit w 0 `extendRun` testBit w 1 `extendRun` testBit w 2 `extendRun` testBit w 3 `extendRun` testBit w 4 `extendRun` testBit w 5 `extendRun` testBit w 6 `extendRun` testBit w 7) (LongestRun False 0 0) l


main :: IO ()
main = do
    bs <- BSL.getContents
    -- print $ longestRun' bs
    print $ longestRun $ bitStream' bs
    -- print $ longestRun $ bits $ fromBS bs
