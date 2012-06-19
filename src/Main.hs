{-# LANGUAGE BangPatterns #-}

module Main where

-- import "binary" Data.Binary
import Data.Bits
import Data.Word
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.IO
import System.Environment (getArgs)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Text.Printf
import Control.Monad (liftM2)

import Huffman
import FreqTree
import TernaryTree
import LZW
import RunLengthEncode
import BWT
import MTF

data Algorithm = Huffman | LZW | RLE | BWT | MTF | Decompress
    deriving (Read, Show, Eq)

extensions :: [(Algorithm, String)]
extensions = [
              (Huffman    , ".huff"   ),
              (LZW        , ".lzw"    ),
              (RLE        , ".rle"    ),
              (BWT        , ".bwt"    ),
              (MTF        , ".mtf"    ),
              (Decompress , ".decoded")
             ]

lzw_numeric_ascii :: TernaryTree Word8
lzw_numeric_ascii = make_initial_tree fromIntegral 0 maxBound

mtf_word8_dict :: [BWTValue Word8]
mtf_word8_dict = EOS : map Value [minBound .. maxBound]

bwt_chunk_length :: Int
bwt_chunk_length = 512
             
-- Takes an algorithm, and a bytestring to compress, and returns
-- the file name suffix, and the bytestring to write
compress :: Algorithm -> BL.ByteString -> Int -> BL.ByteString
compress alg content size =
    case alg of
        Huffman ->
            let (!tree, bits) = huffman_compress (BL.unpack content)
            in runPut $ do
                    putWord64be (fromIntegral size)
                    put tree
                    putLazyByteString (toBS bits)
        LZW ->
            let encoded = lzw_compress lzw_numeric_ascii (BL.unpack content)
                encoded16 = map checkInt encoded
                checkInt :: Int -> Word32
                checkInt n | n <= fromIntegral (maxBound :: Word32) = fromIntegral n
                           | otherwise = error "Main.compress.LZW: encoded value too large\n(This is probably a really big file)"
            in runPut $ do
                    put size
                    mapM_ putWord32be encoded16
        RLE -> 
            let encoded = rle_encode (BL.unpack content)
                write (x,n) | n >= 255 
                                = error $ "Main.compress.RLE: runlength too long: " ++ show (x,n)
                            | otherwise = putWord8 (fromIntegral n) >> putWord8 x
            in runPut $ do
                put size
                mapM_ write encoded
        BWT -> 
            let encoded = rle_encode
                        . concatMap bwt_encode
                        . chunk bwt_chunk_length
                        . BL.unpack $ content
            in runPut $ do
                put size
                mapM_ (\(x,n) -> putWord8 (fromIntegral n) >> put x) encoded
        MTF -> 
            let encoded = rle_encode 
                        . concatMap (map (fromIntegral :: Int -> Word16) . mtf_encode mtf_word8_dict . bwt_encode)
                        . chunk bwt_chunk_length 
                        . BL.unpack $ content
            in runPut $ do
                put size
                mapM_ (\(x,n) -> putWord8 (fromIntegral n) >> putWord16be (fromIntegral x)) encoded
        Decompress -> error "Main.compress: Cannot be used to decompress"
                

decompress :: FilePath -> BL.ByteString -> [Word8]
decompress path content 
    | ".huff" `isSuffixOf` path
    = let (size, tree, encoded) = flip runGet content $ do
            sz <- getWord64be
            tr <- get
            enc <- Data.Binary.Get.getRemainingLazyByteString
            return (fromIntegral sz, tr, enc)
     in takeExactly size (decode_using_tree (tree, bsToBits encoded))
    | ".lzw" `isSuffixOf` path
    = let 
          (size, rest) = flip runGet content (liftM2 (,) get getRemainingLazyByteString)
          toInts :: BL.ByteString -> [Int]
          toInts bs | BL.null bs = []
                    | otherwise = let (n32,rst) = 
                                        flip runGet bs $ 
                                             liftM2 (,) getWord32be getRemainingLazyByteString
                                  in fromIntegral n32 : toInts rst
      in takeExactly size $ lzw_decompress lzw_numeric_ascii (toInts rest)
    | ".rle" `isSuffixOf` path
    = let (size,pairs) = unpair content
      in takeExactly size $ rle_decode pairs
    
    | ".bwt" `isSuffixOf` path
    = let (size,pairs) = unpair content :: (Int,[(BWTValue Word8, Int)])
      in takeExactly size 
         . concatMap bwt_decode
         . chunk (bwt_chunk_length + 1)
         . rle_decode $ pairs
         
    | ".mtf" `isSuffixOf` path
    = let (size,pairs) = unpair content :: (Int,[(Word16, Int)])
      in takeExactly size 
         . concatMap (bwt_decode . mtf_decode mtf_word8_dict)
         . chunk (bwt_chunk_length + 1)
         . map fromIntegral
         . rle_decode $ pairs
    
    | otherwise = error $ "Main.decompress: Unknown filetype: " ++ path


putBWTValues :: Binary a => [BWTValue a] -> Put
putBWTValues xs = mapM_ put . map (mask 0 7) . chunk 8 $ xs
    where mask :: Binary a => Word8 -> Int -> [BWTValue a] -> (Word8,[a])
          mask w n (EOS:xs)     = mask w (n-1) xs
          mask w n (Value x:xs) = let ~(w',ys) = mask (setBit w n) (n-1) xs
                                  in (w',x:ys)
          mask w _ [] = (w,[])

getBWTValues :: Binary a => BL.ByteString -> [BWTValue a]
getBWTValues bs = case runGet getBWTValuesGet bs of
    ~(xs,bs') -> xs ++ getBWTValues bs'

getBWTValuesGet :: Binary a => Get ([BWTValue a],BL.ByteString)
getBWTValuesGet = do
    w <- getWord8
    xs <- go w 7
    bs <- getRemainingLazyByteString
    return (xs,bs)
    where 
        go 0 _ = return []
        go w n | testBit w n = do
                    x <- get
                    xs <- go (clearBit w n) (n-1)
                    return (Value x:xs)
               | otherwise = do
                   xs <- go w (n-1)
                   return (EOS:xs)
    

unpair :: Binary a => BL.ByteString -> (Int,[(a,Int)])
unpair dat = (size, getPairs rest)
    where
        (size, rest) =  flip runGet dat $ liftM2 (,) get getRemainingLazyByteString
        getPairs bs | BL.null bs = [] | otherwise = p : getPairs rst
            where (p,rst) = flip runGet bs $ do
                                n <- getWord8
                                x <- get
                                rst' <- getRemainingLazyByteString
                                return ((x,fromIntegral n), rst')
       
takeExactly :: Int -> [a] -> [a]
takeExactly 0 _ = []
takeExactly n (x:xs) = x : takeExactly (n-1) xs
takeExactly _ [] = error "Main.takeExactly: not enough data"
                
main :: IO ()     
main = do
    (enc:file:_) <- getArgs
    hSetBuffering stdout NoBuffering
    
    hin <- openFile file ReadMode
    size <- fromInteger `fmap` hFileSize hin :: IO Int
    content <- BL.hGetContents hin
    
    let !encoding = read enc
        Just suffix = lookup encoding extensions
    hout <- openFile (file++suffix) WriteMode
    case encoding of
        Decompress -> do
            _ <- printf "\nDecompressing %s\n" file
            _ <- printf "Original size:  %d bytes\n" size
            let res = decompress file content
                resbs = BL.fromChunks . map BS.pack . chunk 8096 $ res
            outsize <- writeLBS hout resbs
            hFlush hout
            _ <- printf "Decompressed size: %d bytes\n" outsize
            _ <- printf "Compression ratio: %3.2f%%\n"
                        (fromIntegral size / fromIntegral outsize * 100 :: Double)
            return ()
            
        _ -> do
            let output = compress encoding content size
            
            _ <- printf "\nCompressing %s using %s\n" file (show encoding) 
            _ <- printf "Original size:     %d bytes\n" size
            
            !rsize <- writeLBS hout output
            hFlush hout
            _ <- printf "Compressed size:   %d bytes\n" rsize
            _ <- printf "Compression ratio: %3.2f%%\n"
                        (fromIntegral rsize / fromIntegral size * 100 :: Double)
            -- case mtree of
            --     Just tree -> print (fmap (chr . fromIntegral) tree)
            --     Nothing -> return ()
            return ()
    hClose hin
    hClose hout
          
        
writeLBS :: Handle -> BL.ByteString -> IO Int
writeLBS h str = go (BL.toChunks str) 0
    where go []     n = return n
          go (x:xs) n = BS.hPut h x >> (go xs $! (n + BS.length x))



toBS :: [Bit] -> BL.ByteString
toBS = BL.fromChunks . map BS.pack . chunk 8096 . toWord8s

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = case splitAt n xs of
    (ys,zs) -> ys : chunk n zs
    
toWord8s :: [Bit] -> [Word8]
toWord8s xs = case splitAt 8 xs of
    (ys,[]) -> [mkw8 ys]
    (ys,zs) -> mkw8 ys : toWord8s zs

mkw8 :: [Bit] -> Word8
mkw8 xs = foldl (\n (p,b) -> if b then setBit n p else n) 0 
        $ zip [7,6..0] (map (==One) xs)
        
boolToBit :: Bool -> Bit
boolToBit True = One
boolToBit False = Zero

fromw8 :: Word8 -> [Bit]
fromw8 n = map (boolToBit . testBit n) [7,6..0]

bsToBits :: BL.ByteString -> [Bit]
bsToBits bs = concatMap fromw8 (BL.unpack bs)








