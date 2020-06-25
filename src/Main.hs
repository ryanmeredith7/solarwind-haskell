module Main (main) where

import Prelude hiding (take,mapM_)

import Omni (readOmni)
import Jumps (findJumps,getJumps,flt2dbl)

import Data.Vector.Unboxed (take,mapM_)

import System.Environment (getArgs)

main :: IO ()
main = do
    year <- read . head <$> getArgs
    xs <- flt2dbl . take 2880 <$> readOmni year
    mapM_ print $ getJumps $ findJumps xs
