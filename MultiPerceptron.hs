--{-# LANGUAGE BangPatterns,  #-}
module MultiPerceptron where

--import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector.Unboxed as V
-- import Test.QuickCheck

class SupervisedLearner l  where
    -- l is a learner function
     learn :: (l-> Output n) -> Input  -> Output n

data Input  = Supervised { inInput :: V.Vector  Double, inOutput :: V.Vector Double } 
            | Unsupervised { inOutput :: V.Vector Double }


-- Regression and Classification should only return the single output  
-- Kclass needs to have multiple outputs 
data Output n  =  Regression{ outReg ::  n }  
               | Kclass { outClass :: V.Vector n }  
               | Bernoulli { outBer :: (V.Vector n, V.Vector n) }
               deriving (Show)

type Weights = V.Vector Double



-- internally used in the idea of a perceptron being a comp machine
calcWeights :: Input -> Weights -> Double
calcWeights i w  =  V.sum $ V.zipWith (*) (inInput i) w 

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1 + exp (-x))

kClassOutput :: V.Vector Double -> Output Double
kClassOutput i 
    | V.null i = Kclass $ V.singleton 1.0
    | otherwise  =  Kclass $ V.map (\x-> exp x / total) i 
        where total =  V.foldr' (\g h -> exp g + h) 0 i

-- simple sanity check
prop_sumToOne b =  (num  < 1.001) && (num > 0.99) 
    where num = V.sum . outClass $ kClassOutput $ V.fromList b 


-- testing stuff
b = V.fromList [1.0,2.0,3.0,4.0,5.0] 
o = V.fromList [1.3,2.2,3.2,4.4,5.4]
a =  Supervised {inInput=b,inOutput=o}



{-
parse :: L.ByteString -> V.Vector Int
parse = V.unfoldr step
    where
     step !s = case L.readInt s of
        Nothing       -> Nothing
        Just (!k, !t) -> Just (k, L.tail t)
-}
