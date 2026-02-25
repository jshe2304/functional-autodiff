-- | Model-agnostic gradient descent training.

{-# LANGUAGE RankNTypes #-}

module Train (gradStep, train) where

import AutoDiff (grad)

-- | One step of gradient descent.
gradStep :: Double -> (forall a. Floating a => [a] -> a) -> [Double] -> [Double]
gradStep lr loss params =
    let g = grad loss params
    in zipWith (\p gi -> p - lr * gi) params g

-- | Run gradient descent, printing progress every 100 steps.
train
    :: (forall a. Floating a => [a] -> a)
    -> Double
    -> Int
    -> [Double]
    -> IO [Double]
train loss lr steps = go 0
  where
    go i params
        | i >= steps = return params
        | otherwise  = do
            let params' = gradStep lr loss params
                l       = loss params'
            if i `mod` 100 == 0
                then putStrLn $ "step " ++ show i ++ "  loss=" ++ show l
                else return ()
            go (i + 1) params'
