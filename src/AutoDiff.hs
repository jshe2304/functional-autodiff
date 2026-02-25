-- | Main module for the automatic differentiation library.
module AutoDiff
    ( -- * Types
      Dual(..)
      -- * Operations
    , var
    , constant
    , diff
    , grad
    ) where

import AutoDiff.Types
import AutoDiff.Forward
