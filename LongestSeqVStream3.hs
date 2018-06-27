{-# LANGUAGE CPP #-}
#define PHASE_FUSED [1]
#define PHASE_INNER [0]
#define INLINE_FUSED INLINE PHASE_FUSED
#define INLINE_INNER INLINE PHASE_INNER
module Main where

import           Control.Monad.Identity            (Identity)
import           Data.Bits                         (shiftR, (.&.))
import qualified Data.ByteString.Lazy              as BSL
import           Data.Functor.Identity             (runIdentity)
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

data Step2 s = Step2 s !Bool !Int

-- convert a Stream of Bool into a Stream of lengths of runs
{-# INLINE_FUSED mkRuns #-}
mkRuns :: Monad m => S.Stream m Bool -> S.Stream m Int
mkRuns (S.Stream step s) = S.Stream step' (Step2 s True 1) where
    {-# INLINE_INNER step' #-}
    step' (Step2 s' current' count') = do
        next <- step s'
        case next of
            S.Done -> return S.Done
            S.Skip s'' -> return $ S.Skip (Step2 s'' current' count')
            S.Yield a s'' -> return $
                if a == current'
                then S.Skip (Step2 s'' current' (count'+1))
                else S.Yield count' (Step2 s'' (not current') 1)

main :: IO ()
main = do
    bs <- BSL.getContents
    print $ runIdentity $ S.foldl' max 0 $ mkRuns $ mkBitstream bs

