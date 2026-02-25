-- | Simple linear regression using gradient descent powered by AutoDiff.
--
-- We fit y = m*x + b to noisy data sampled from a given function on [-4, 4].
-- Gradients are computed automatically via forward-mode AD.
--
-- Usage:  cabal run linear-regression

module Main where

import DataGen (generateLinearData)
import Train   (train)

-- | Mean squared error for a linear model y = m*x + b.
mse :: Fractional a => [(a, a)] -> [a] -> a
mse dataset params =
    let m = params !! 0
        b = params !! 1
        n = fromIntegral (length dataset)
        errs = [ (y - (m * x + b)) ^ (2 :: Int) | (x, y) <- dataset ]
    in sum errs / n

main :: IO ()
main = do
    let trueM  = 3.0
        trueB  = 1.0
        trueF x = trueM * x + trueB
        sigma  = 0.5
        n      = 50

    let dataset = generateLinearData trueF (-4) 4 n sigma

    putStrLn "=== Linear Regression with AutoDiff ==="
    putStrLn ""
    putStrLn $ "True model:  y = " ++ show trueM ++ "*x + " ++ show trueB
    putStrLn $ "Noise:       N(0, " ++ show sigma ++ "^2)"
    putStrLn $ "Samples:     " ++ show n ++ " on [-4, 4]"
    putStrLn ""

    let initParams = [0.0, 0.0]
        lr         = 0.001
        steps      = 1000

    putStrLn $ "Initial params: m=" ++ show (initParams !! 0)
                                     ++ ", b=" ++ show (initParams !! 1)
    putStrLn $ "Learning rate:  " ++ show lr
    putStrLn $ "Steps:          " ++ show steps
    putStrLn ""

    let loss :: Floating a => [a] -> a
        loss = mse (map (\(x, y) -> (realToFrac x, realToFrac y)) dataset)

    finalParams <- train loss lr steps initParams

    let m = finalParams !! 0
        b = finalParams !! 1

    putStrLn ""
    putStrLn $ "Learned:  m=" ++ show m ++ ", b=" ++ show b
    putStrLn $ "Expected: m≈" ++ show trueM ++ ", b≈" ++ show trueB
