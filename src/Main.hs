module Main (main) where

import Omni (getOmni)

import System.Environment (getArgs)

main :: IO ()
main = read <$> head <$> getArgs >>= getOmni
