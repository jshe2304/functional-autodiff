-- | Data generation utilities for examples.

module DataGen (generateLinearData, generateBimodalData, gaussians) where

import System.Random (mkStdGen, randoms)

-- | Box-Muller transform: pairs of uniform samples -> Gaussian samples.
gaussians :: Double -> Double -> [Double]
gaussians mu sigma = go us
  where
    us = randoms (mkStdGen 42) :: [Double]
    go (u1:u2:rest) =
        let r  = sqrt (-2 * log u1)
            th = 2 * pi * u2
        in (mu + sigma * r * cos th) : (mu + sigma * r * sin th) : go rest
    go _ = []

-- | Generate n noisy samples from f on [lo, hi] with Gaussian noise N(0, sigma^2).
generateLinearData :: (Double -> Double) -> Double -> Double -> Int -> Double -> [(Double, Double)]
generateLinearData f lo hi n sigma =
    let step = (hi - lo) / fromIntegral (n - 1)
        xs   = [lo + fromIntegral i * step | i <- [0 .. n - 1]]
        noise = gaussians 0 sigma
    in zipWith (\x eps -> (x, f x + eps)) xs noise

-- | Generate binary classification data: n samples per class from two Gaussians.
-- Class 0 is drawn from N(mu0, sigma^2), class 1 from N(mu1, sigma^2).
-- Returns [(feature, label)] where label is 0 or 1.
generateBimodalData :: Double -> Double -> Double -> Int -> [(Double, Double)]
generateBimodalData mu0 mu1 sigma n =
    let noise0 = take n (gaussians mu0 sigma)
        noise1 = take n (drop (2 * n) (gaussians mu1 sigma))
    in  [(x, 0) | x <- noise0] ++ [(x, 1) | x <- noise1]
