-- | Logistic regression using gradient descent powered by AutoDiff.
--
-- We fit P(y=1|x) = sigmoid(w*x + b) to binary classification data
-- generated from two Gaussians. Gradients are computed automatically
-- via forward-mode AD.
--
-- Usage:  cabal run logistic-regression

module Main where

import DataGen (generateBimodalData)
import Train   (train)

-- | Sigmoid (logistic) function.
sigmoid :: Floating a => a -> a
sigmoid z = 1 / (1 + exp (negate z))

-- | Binary cross-entropy loss for logistic regression.
-- Model: P(y=1|x) = sigmoid(w*x + b), params = [w, b].
bce :: Floating a => [(a, a)] -> [a] -> a
bce dataset params =
    let w = params !! 0
        b = params !! 1
        n = fromIntegral (length dataset)
        eps = 1e-7
        losses =
            [ negate (y * log (p + eps) + (1 - y) * log (1 - p + eps))
            | (x, y) <- dataset
            , let p = sigmoid (w * x + b)
            ]
    in sum losses / n

-- | Classify a point: 1 if sigmoid(w*x + b) >= 0.5, else 0.
classify :: Double -> Double -> Double -> Double
classify w b x = if sigmoid (w * x + b) >= 0.5 then 1 else 0

-- | Compute classification accuracy.
accuracy :: Double -> Double -> [(Double, Double)] -> Double
accuracy w b dataset =
    let correct = length [() | (x, y) <- dataset, classify w b x == y]
    in fromIntegral correct / fromIntegral (length dataset)

main :: IO ()
main = do
    let mu0      = -2.0
        mu1      =  2.0
        sigma    =  1.0
        n        =  50
        boundary = (mu0 + mu1) / 2

    let dataset = generateBimodalData mu0 mu1 sigma n

    putStrLn "=== Logistic Regression with AutoDiff ==="
    putStrLn ""
    putStrLn $ "True model:  P(y=1|x) = sigmoid(w*x + b)"
    putStrLn $ "             class 0 ~ N(" ++ show mu0 ++ ", " ++ show sigma ++ "^2)"
    putStrLn $ "             class 1 ~ N(" ++ show mu1 ++ ", " ++ show sigma ++ "^2)"
    putStrLn $ "             decision boundary: x=" ++ show boundary
    putStrLn $ "Noise:       σ=" ++ show sigma
    putStrLn $ "Samples:     " ++ show n ++ " per class (" ++ show (2 * n)
                               ++ " total) on ℝ"
    putStrLn ""

    let initParams = [0.0, 0.0]
        lr         = 0.1
        steps      = 1000

    putStrLn $ "Initial params: w=" ++ show (initParams !! 0)
                                     ++ ", b=" ++ show (initParams !! 1)
    putStrLn $ "Learning rate:  " ++ show lr
    putStrLn $ "Steps:          " ++ show steps
    putStrLn ""

    let loss :: Floating a => [a] -> a
        loss = bce (map (\(x, y) -> (realToFrac x, realToFrac y)) dataset)

    finalParams <- train loss lr steps initParams

    let w = finalParams !! 0
        b = finalParams !! 1

    putStrLn ""
    putStrLn $ "Learned:  w=" ++ show w ++ ", b=" ++ show b
    putStrLn $ "Decision boundary: x=" ++ show (negate b / w)
                ++ " (expected ≈" ++ show boundary ++ ")"
    putStrLn $ "Training accuracy: " ++ show (accuracy w b dataset * 100) ++ "%"
