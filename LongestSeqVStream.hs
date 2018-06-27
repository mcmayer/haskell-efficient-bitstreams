{-# LANGUAGE CPP #-}
#define PHASE_FUSED [1]
#define PHASE_INNER [0]
#define INLINE_FUSED INLINE PHASE_FUSED
#define INLINE_INNER INLINE PHASE_INNER
module Main where

import           Data.Bits                         (shiftR, (.&.))
import qualified Data.ByteString.Lazy              as BSL
import qualified Data.Vector.Fusion.Stream.Monadic as S
import           Data.Word8                        (Word8)

data Step = Step BSL.ByteString !Word8 !Word8

{-# INLINE_FUSED mkBitstream #-}
mkBitstream :: Monad m => BSL.ByteString -> S.Stream m Bool
mkBitstream bs' = S.Stream step (Step bs' 0 0) where
    {-# INLINE_INNER step #-}
    step (Step bs w n) | n==0 = case (BSL.uncons bs) of
                            Nothing        -> return S.Done
                            Just (w', bs') -> return $
                                S.Yield (w' .&. 1 == 1) (Step bs' (w' `shiftR` 1) 7)
                       | otherwise = return $
                                S.Yield (w .&. 1 == 1) (Step bs (w `shiftR` 1) (n-1))

data LongestRun = LongestRun !Bool !Int !Int

{-# INLINE_INNER extendRun #-}
extendRun :: LongestRun -> Bool -> LongestRun
extendRun (LongestRun previous run longest) x  = LongestRun x current (max current longest)
    where
    current = if x == previous then run + 1 else 1

{-# INLINE_FUSED longestRun #-}
longestRun :: S.Stream IO Bool -> IO Int
longestRun s = do
    (LongestRun _ _ longest) <- S.foldl' extendRun (LongestRun False 0 0) s
    -- (LongestRun _ _ longest) <- S.foldr (flip extendRun) (LongestRun False 0 0) s
    return longest

main :: IO ()
main = do
    bs <- BSL.getContents
    l <- longestRun $ mkBitstream bs
    print l

