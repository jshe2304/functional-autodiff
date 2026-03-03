# Functional Auto-Differentiation

A forward-mode automatic differentiation library in Haskell using dual numbers. Below is a guide to running all the code. A nice looking TeX report and the slides used in the presentation are in `tex/`. 

## Project Structure

- `src/AutoDiff/Types.hs` - `Dual a` type and typeclass instances (`Num`, `Fractional`, `Floating`)
- `src/AutoDiff/Forward.hs` - Forward-mode AD operations: `var`, `constant`, `diff`, `grad`, `(^!)`
- `src/AutoDiff.hs` - Main module, re-exports everything
- `test/AutoDiffSpec.hs` - HSpec test suite
- `app/LinearRegression.hs` - Linear regression example using AD-computed gradients
- `app/LogisticRegression.hs` - Logistic regression example using AD-computed gradients
- `visualization/` - Data generation and plotting scripts for derivative and gradient field visualizations
- `tex/` - TeX report and presentation slides

## Build

```bash
cabal build
```

## Regression Examples

Both examples use AD to compute gradients for gradient descent training.

### Linear Regression

Generates noisy samples from `y = m*x + b` and fits the parameters using MSE loss.

```bash
# With default parameters (slope=3, intercept=1)
cabal run linear-regression

# With custom parameters
cabal run linear-regression -- <slope> <intercept>
cabal run linear-regression -- 3.0 1.0
```

### Logistic Regression

Generates binary classification data from two Gaussians and fits a logistic model using cross-entropy loss. Reports the fitted weights, decision boundary, and classification accuracy.

```bash
# With default parameters (mu0=-2, mu1=2, sigma=1)
cabal run logistic-regression

# With custom parameters
cabal run logistic-regression -- <mu0> <mu1> <sigma>
cabal run logistic-regression -- -2.0 2.0 1.0
```

## Visualizations

### 1D Derivative Plots

Generate CSV data for four test functions (polynomial, sinusoidal, exponential, composite) showing `f(x)` and its first three derivatives, then plot them.

```bash
cabal run gen-data
python3 visualization/plot.py
```

Output: one PNG per function in `visualization/images/` (e.g., `polynomial.png`, `sinusoidal.png`).

### 2D Gradient Field Plot

Generate a gradient field for `f(x, y) = sin(x) * cos(y)` over a 50x50 grid, then plot a 3D surface and a contour plot with gradient vectors.

```bash
cabal run gen-gradient-data
python3 visualization/plot_gradient.py
```

Output: `visualization/images/gradient_field.png`.

## Run Everything

```bash
cabal build
cabal test
cabal run linear-regression
cabal run logistic-regression
cabal run gen-data
cabal run gen-gradient-data
python3 visualization/plot.py
python3 visualization/plot_gradient.py
```
