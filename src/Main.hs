module Main where

import System.Environment (getArgs)

main :: IO ()
main = head <$> getArgs >>= readFile >>= mapM_ (print . (readOmni :: String -> Float)) . lines

readOmni :: (RealFloat a, Read a) => String -> a
readOmni line = case read $ take 6 $ drop 171 $ line of
    99.99 -> 0 / 0 -- NaN
    x -> x
