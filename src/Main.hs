module Main (main) where

import Prelude hiding (lines)

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Char8 (unpack,lines)
import Data.IORef

import Control.Monad (unless,forM_)

import Text.Printf (printf)

import Network.HTTP.Simple

import System.IO
import System.Environment (getArgs)

import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (sizeOf,poke)

main :: IO ()
main = read <$> head <$> getArgs >>= getOmni

omniRequest :: Request
omniRequest = flip ($) defaultRequest
    $ setRequestHost (pack "cdaweb.gsfc.nasa.gov")
    . setRequestSecure True
    . setRequestPort 443

getOmni :: Int -> IO ()
getOmni year = do
    unless (1995 <= year) $ fail "No omni data from before 1995."
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
            poke ptr $ readLine $ unpack ln
            hPutBuf h ptr 4
            modifyIORef' k succ
        n <- readIORef k
        hSetPosn start
        with n $ \ptr -> hPutBuf h ptr intBytes

readLine :: String -> Float
readLine line = case read $ take 6 $ drop 171 $ line of
    99.99 -> 0 / 0 -- NaN
    x -> x
