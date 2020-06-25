module Jumps (findJumps,getJumps,flt2dbl) where

import Prelude hiding (filter,length,replicate,take,(++),splitAt)

import Data.Function (on)
import Data.Ord (comparing)
import Data.Foldable (foldMap')
import Data.Bifunctor (bimap)
import Data.Vector.Unboxed (filter,slice,length,convert,splitAt)
import Data.Vector (replicate,take,force,modify,(++),maxIndexBy)
import Data.Vector.Mutable (write)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

import Numeric.Sum (sumVector,kbn)
import Statistics.Sample (mean)
import GHC.Float (float2Double)

data Partition = Partition
    { start :: Int
    , num :: Int
    , level :: Double
    , _loss :: Double
    , _split :: Int
    , diff :: Double
    , _levels :: (Double,Double)
    , _losses :: (Double,Double)
    } deriving Show

data Split = Split
    { _num :: Int
    , _lvls :: (Double,Double)
    , _lsses :: (Double,Double)
    , loss :: Double
    }

instance Eq Split where
    (==) = (==) `on` loss

instance Ord Split where
    (<=) = (<=) `on` loss

instance Semigroup Split where
    (<>) = min

instance Monoid Split where
    mempty = Split 0 (0,0) (0,0) (1 / 0) -- 1 / 0 = Infinity

mkPart :: U.Vector Double -> Int -> Double -> Double -> Partition
mkPart xs a lvl lss
    | m <= 1 = Partition a m lvl lss a 0 (0,0) (0,0)
    | otherwise =
        case flip foldMap' [1..m-1] $ \i ->
            let filt = filter (not . isNaN)
                (x1,x2) = bimap filt filt $ splitAt i xs
                m1 = mean x1
                m2 = mean x2
                s1 = sumVector kbn $ U.map (\x -> (x - m1) ^ (2 :: Int)) x1
                s2 = sumVector kbn $ U.map (\x -> (x - m2) ^ (2 :: Int)) x2
            in Split i (m1,m2) (s1,s2) (s1 + s2)
        of Split n lvls lses lss' -> Partition a m lvl lss n (lss - lss') lvls lses
    where m = length xs

splitPart :: U.Vector Double -> Partition -> (Partition,Partition)
splitPart inData (Partition a m _ _ n _ (lvl1,lvl2) (lss1,lss2)) =
    ( mkPart (slice a n inData) a lvl1 lss1
    , mkPart (slice (a+n) (m-n) inData) (a+n) lvl2 lss2
    )

findJumps :: U.Vector Double -> V.Vector Partition
findJumps inData =
    let noNaN = filter (not . isNaN) inData
        lev = mean noNaN
        los = sumVector kbn $ U.map (\x -> (x - lev) ^ (2 :: Int)) noNaN
        vec = replicate 50 $ mkPart inData 0 lev los
    in loop vec 1 0 (diff $ vec V.! 0)
    where
        loop :: V.Vector Partition -> Int -> Int -> Double -> V.Vector Partition
        loop parts k splitInd lossDiff
            | lossDiff <= 120 = force $ take k parts
            | k `mod` 50 == 0 = loop (parts ++ take 50 parts) k splitInd lossDiff
            | otherwise =
                let (x,y) = splitPart inData $ parts V.! splitInd
                    parts' = modify (\v -> write v splitInd x >> write v k y) parts
                    splitInd' = maxIndexBy (comparing diff) parts'
                    lossDiff' = diff $ parts' V.! splitInd'
                in loop parts' (k + 1) splitInd' lossDiff'

getJumps :: V.Vector Partition -> U.Vector (Double,Int,Int)
getJumps = convert . (V.map $ (,,) <$> level <*> start <*> num)

flt2dbl :: U.Vector Float -> U.Vector Double
flt2dbl = U.map float2Double
