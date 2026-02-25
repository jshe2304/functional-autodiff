-- | Core types for automatic differentiation.
module AutoDiff.Types
    ( Dual(..)
    ) where

-- | A dual number consists of a value and its derivative.
-- Used for forward-mode automatic differentiation.
data Dual a = Dual
    {
        primal  :: !a,  -- ^ The value 
        tangent :: !a  -- ^ The derivative
    } deriving (Show, Eq)

instance Num a => Num (Dual a) where
    Dual x x' + Dual y y' = Dual (x + y) (x' + y')
    Dual x x' - Dual y y' = Dual (x - y) (x' - y')
    Dual x x' * Dual y y' = Dual (x * y) (x' * y + x * y')
    negate (Dual x x')    = Dual (negate x) (negate x')
    abs (Dual x x')       = Dual (abs x) (x' * signum x)
    signum (Dual x _)     = Dual (signum x) 0
    fromInteger n         = Dual (fromInteger n) 0

instance Fractional a => Fractional (Dual a) where
    Dual x x' / Dual y y' = Dual (x / y) ((x' * y - x * y') / (y * y))
    fromRational r        = Dual (fromRational r) 0

instance Floating a => Floating (Dual a) where
    pi                  = Dual pi 0
    exp (Dual x x')     = Dual (exp x) (x' * exp x)
    log (Dual x x')     = Dual (log x) (x' / x)
    sin (Dual x x')     = Dual (sin x) (x' * cos x)
    cos (Dual x x')     = Dual (cos x) (negate x' * sin x)
    tan (Dual x x')     = Dual (tan x) (x' / (cos x * cos x))
    sqrt (Dual x x')    = Dual (sqrt x) (x' / (2 * sqrt x))
    Dual x x' ** Dual y y' = Dual (x ** y) (x ** y * (y' * log x + y * x' / x))
    logBase (Dual b b') (Dual x x') = Dual (logBase b x) ((x' / x - log x * b' / b) / log b)
