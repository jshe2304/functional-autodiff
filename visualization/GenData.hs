-- | Generate CSV files with function values and derivatives up to third order.
module Main where

import AutoDiff

-- | Sample a function and its first three derivatives over a range, writing a CSV file.
writeSample :: String -> (Double, Double) -> Int -> (forall a. Floating a => a -> a) -> IO ()
writeSample name (lo, hi) n f = do
    let path = "visualization/data/" ++ name ++ ".csv"
        header = "x,f(x),f'(x),f''(x),f'''(x)"
        d1 = diff f
        d2 = (diff . diff) f
        d3 = (diff . diff . diff) f
        rows = [ show x ++ "," ++ show (f x) ++ "," ++ show (d1 x) ++ "," ++ show (d2 x) ++ "," ++ show (d3 x)
               | i <- [0 .. n - 1]
               , let x = lo + (hi - lo) * fromIntegral i / fromIntegral (n - 1)
               ]
    writeFile path (unlines (header : rows))
    putStrLn $ "Wrote " ++ path

main :: IO ()
main = do
    -- Polynomial: x^4 - 3x^2 + 2
    writeSample "polynomial" (-2.5, 2.5) 200 (\x -> x*x*x*x - 3*x*x + 2)
    -- Sinusoidal: sin
    writeSample "sinusoidal" (-2*pi, 2*pi) 200 sin
    -- Exponential: exp
    writeSample "exponential" (-2, 3) 200 exp
    -- Composite: exp(-x^2) (Gaussian)
    writeSample "composite" (-3, 3) 200 (\x -> exp (negate (x*x)))
