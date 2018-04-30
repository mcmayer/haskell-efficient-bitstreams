module Main where

    import           Data.Bits            (shiftR, (.&.))
    import qualified Data.ByteString.Lazy as BSL
    import           Data.Word8           (Word8)
    
    splitByte :: Word8 -> [Bool]
    splitByte w = map (\i-> (w `shiftR` i) .&. 1 == 1) [0..7]
    
    bitStream :: BSL.ByteString -> [Bool]
    bitStream bs = concat $ map splitByte (BSL.unpack bs)
    
    main :: IO ()
    main = do
        bs <- BSL.getContents
        print $ BSL.length bs
    