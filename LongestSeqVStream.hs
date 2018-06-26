{-# LANGUAGE CPP #-}
#define PHASE_FUSED [1]
#define PHASE_INNER [0]
#define INLINE_FUSED INLINE PHASE_FUSED
#define INLINE_INNER INLINE PHASE_INNER
module Main where

import           Control.Monad.Identity            (Identity)
import           Data.Bits                         (shiftR, (.&.))
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as BSL
import qualified Data.Vector.Fusion.Stream.Monadic as S
import           Data.Word8                        (Word8)



-- {-# INLINE mkWord8Stream #-}
mkWord8Stream :: Monad m => BSL.ByteString -> S.Stream m Word8
mkWord8Stream = S.Stream step where
    -- {-# INLINE_INNER step #-}
    step bs = case (BSL.uncons bs) of
                Nothing        -> return S.Done
                Just (w, bs'') -> return $ S.Yield w bs''

-- {-# INLINE mkBits #-}
mkBits :: Monad m => Word8 -> S.Stream m Bool
mkBits w' = S.Stream step (w', 8::Word8) where
    {-# INLINE_INNER step #-}
    step (w,n) | n==0 = return S.Done
               | otherwise = return $ S.Yield (w .&. 1 == 1) (w `shiftR` 1, n-1)

data Step = Step BSL.ByteString !Word8 !Word8

-- {-# INLINE mkBitstream' #-}
mkBitstream' :: Monad m => BSL.ByteString -> S.Stream m Bool
mkBitstream' bs' = S.Stream step (Step bs' 0 0) where
    {-# INLINE_INNER step #-}
    step (Step bs w n) | n==0 = case (BSL.uncons bs) of
                            Nothing        -> return S.Done
                            Just (w', bs') -> return $ S.Yield (w' .&. 1 == 1) (Step bs' (w' `shiftR` 1) 7)
                       | otherwise = return $ S.Yield (w .&. 1 == 1) (Step bs (w `shiftR` 1) (n-1))

-- {-# INLINE mkBitstream #-}
mkBitstream :: Monad m => BSL.ByteString -> S.Stream m Bool
mkBitstream bs = S.concatMap mkBits (mkWord8Stream bs)

data LongestRun = LongestRun !Bool !Int !Int

-- {-# INLINE extendRun #-}
extendRun :: LongestRun -> Bool -> LongestRun
extendRun (LongestRun previous run longest) x  = LongestRun x current (max current longest)
    where
    current = if x == previous then run + 1 else 1

-- {-# INLINE extendRun' #-}
extendRun' :: Bool -> LongestRun -> LongestRun
extendRun' x (LongestRun previous run longest) = LongestRun x current (max current longest)
    where
    current = if x == previous then run + 1 else 1

-- {-# INLINE longestRun #-}
longestRun :: S.Stream IO Bool -> IO Int
longestRun s = do
    (LongestRun _ _ longest) <- S.foldl' extendRun (LongestRun False 0 0) s
    -- (LongestRun _ _ longest) <- S.foldr extendRun' (LongestRun False 0 0) s
    return longest

main :: IO ()
main = do
    -- l <- S.toList $ mkBitstream' (BSL.pack [1,2])
    -- print l
    bs <- BSL.getContents
    l <- longestRun $ mkBitstream' bs
    print l

