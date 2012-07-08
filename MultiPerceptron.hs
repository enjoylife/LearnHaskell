--{-# LANGUAGE BangPatterns,  #-}
module MultiPerceptron where

--import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector.Unboxed as V

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



-- the input that we need
calcWeights :: Input -> Weights -> Double
calcWeights i w  =  V.sum $ V.zipWith (*) (inInput i) w 

sigmoid :: Double -> Double
sigmoid x = 1.0 / (1 + exp (-x))

kClassOutput :: V.Vector Double -> Output Double
kClassOutput  i =  Kclass $ V.map  (\x-> exp x / total) i 
        where total =  V.foldr' (\a b -> ((exp a) + b)) 0 i


-- testing stuff
b = V.fromList [1.0,2.0,3.0,4.0,5.0] 
o = V.fromList [1.3,2.2,3.2,4.4,5.4]
a =  Supervised {inInput=b,inOutput=o}

g = (V.sum . outClass $ kClassOutput b ) == 1.0

--threshold = undefined

{-
parse :: L.ByteString -> V.Vector Int
parse = V.unfoldr step
    where
     step !s = case L.readInt s of
        Nothing       -> Nothing
        Just (!k, !t) -> Just (k, L.tail t)
-}
