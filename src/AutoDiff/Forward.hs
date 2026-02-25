-- | Forward-mode automatic differentiation.
module AutoDiff.Forward
    ( var
    , constant
    , diff
    , grad
    ) where

import AutoDiff.Types
import Data.Traversable (mapAccumL)

-- | Create a variable for differentiation.
var :: Num a => a -> Dual a
var x = Dual x 1

-- | Create a constant (derivative is zero).
constant :: Num a => a -> Dual a
constant x = Dual x 0

-- | Differentiation combinator returning the derivative as a first-class function.
-- Compose for higher-order derivatives: @diff . diff $ f@ is the second derivative.
diff :: Num a => (Dual a -> Dual a) -> (a -> a)
diff f = \x -> tangent (f (var x))


-- | Compute the gradient of a function over any Traversable structure.
-- Each partial derivative is computed by seeding that position with tangent 1
-- and all others with 0 (standard forward-mode approach).
grad :: (Num a, Traversable f) => (f (Dual a) -> Dual a) -> f a -> f a
grad f xs = snd $ mapAccumL step (0 :: Int) xs
  where
    step i _ = (i + 1, tangent (f (snd $ mapAccumL seed (0 :: Int) xs)))
      where
        seed j x = (j + 1, Dual x (if i == j then 1 else 0))

