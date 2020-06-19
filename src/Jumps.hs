module Jumps () where

-- import Prelude (Int,Double)

-- import Data.Eq
-- import Data.Ord
-- import Data.Function

-- import Data.Vector.Generic
-- import Data.Vector.Unboxed as U
-- import Data.Vector as V
 
-- data Partition = Partition
--     { start :: Int
--     , finish :: Int
--     , level :: Double
--     , loss :: Double
--     , split :: Int
--     , diff :: Double
--     , levels :: (Double,Double)
--     , losses :: (Double,Double)
--     }

-- instance Eq Partition where
--     (==) = (==) `on` loss
--     (/=) = (/=) `on` loss

-- instance Ord Partition where
--     compare = comparing level
--     (<) = (<) `on` loss
--     (>) = (>) `on` loss
--     (<=) = (<=) `on` loss
--     (>=) = (>=) `on` loss

-- mkPart :: U.Vector Double -> Int -> Int -> Double -> Double -> Partition
-- mkPart inData a b lvl lss = Partition 0 0 0 0 0 0 (0,0) (0,0)


