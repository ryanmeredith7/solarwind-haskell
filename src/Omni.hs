module Omni (getOmni, readOmni) where

import Prelude hiding (lines,take,drop)

import Data.IORef (newIORef,modifyIORef',readIORef)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Char8 (ByteString,unpack,lines,take,drop)
import Data.Vector.Unboxed (Vector,replicateM)

import Control.Monad (when,unless,forM_)

import Text.Printf (printf)

import Network.HTTP.Simple

import System.IO

import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (sizeOf,poke,peek)

getOmni :: Int -> IO ()
getOmni year = do
    when (year < 1995) $ fail "no omni data from before 1995"
    let path = printf "/pub/data/omni/high_res_omni/modified/omni_min%4d.asc" year
        request = setRequestPath (pack path) omniRequest
        localPath = printf "data/omni/yearly/%4d.bin" year
        intBytes = sizeOf (0 :: Int)
    lns <- lines . getResponseBody <$> httpLBS request
    k <- newIORef (0 :: Int)
    withBinaryFile localPath WriteMode $ \h -> do
        start <- hGetPosn h
        with (0 :: Int) $ \ptr -> hPutBuf h ptr intBytes
        alloca $ \ptr -> forM_ lns $ \ln -> do
            poke ptr $ readLine ln
            hPutBuf h ptr 4
            modifyIORef' k succ
        n <- readIORef k
        hSetPosn start
        with n $ \ptr -> hPutBuf h ptr intBytes

omniRequest :: Request
omniRequest = flip ($) defaultRequest
    $ setRequestHost (pack "cdaweb.gsfc.nasa.gov")
    . setRequestSecure True
    . setRequestPort 443

readLine :: ByteString -> Float
readLine line = case read $ unpack $ take 6 $ drop 171 line of
    99.99 -> 0 / 0 -- NaN
    x -> x

readOmni :: Int -> IO (Vector Float)
readOmni year = do
    when (year < 1995) $ fail "no omni data from before 1995"
    let path = printf "data/omni/yearly/%4d.bin" year
        intBytes = sizeOf (0 :: Int)
    withBinaryFile path ReadMode $ \h -> do
        n <- alloca $ \ptr -> do
            bytes <- hGetBuf h ptr intBytes
            unless (bytes == intBytes) $ fail "unexpected end of file"
            peek ptr
        alloca $ \ptr -> replicateM n $ do
            bytes <- hGetBuf h ptr 4
            unless (bytes == 4) $ fail "unexpected end of file"
            peek ptr
