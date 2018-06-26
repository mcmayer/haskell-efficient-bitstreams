module Main where

    import           Control.Monad.Identity    (Identity)
    import           Data.Bits                 (shiftR, (.&.))
    import           Data.Word8                (Word8)
    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Lazy as BSL
    import qualified Data.Vector.Fusion.Stream.Monadic as S

    mkStream :: Monad m => BSL.ByteString -> S.Stream m Word8
    mkStream = S.Stream step where
        step bs = case (BSL.uncons bs) of
                    Nothing -> return S.Done
                    Just (w, bs'') -> return $ S.Yield w bs''


    main :: IO ()
    main = do
        bs <- BSL.getContents
        sum' <- S.foldl' (+) 0 (mkStream bs)
        print sum'
    
    