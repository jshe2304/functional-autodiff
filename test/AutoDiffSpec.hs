module AutoDiffSpec (spec) where

import Test.Hspec
import AutoDiff

-- Helper to check floating-point derivatives within tolerance
shouldBeApprox :: Double -> Double -> Expectation
shouldBeApprox actual expected =
    abs (actual - expected) < 1e-10 `shouldBe` True

spec :: Spec
spec = do
    describe "diff" $ do
        it "differentiates x^2 to 2x" $ do
            diff (\x -> x * x) 3 `shouldBe` (6 :: Integer)

        it "differentiates x^3 to 3x^2" $ do
            diff (\x -> x * x * x) 2 `shouldBe` (12 :: Integer)

        it "differentiates constants to 0" $ do
            diff (const (constant 5)) 10 `shouldBe` (0 :: Integer)

    describe "Dual arithmetic" $ do
        it "adds dual numbers correctly" $ do
            var 3 + var 4 `shouldBe` (Dual 7 2 :: Dual Integer)

        it "multiplies dual numbers correctly" $ do
            var 3 * constant 2 `shouldBe` (Dual 6 2 :: Dual Integer)

        it "subtracts dual numbers correctly" $ do
            var 5 - var 3 `shouldBe` (Dual 2 0 :: Dual Integer)

        it "negates dual numbers correctly" $ do
            negate (var 3) `shouldBe` (Dual (-3) (-1) :: Dual Integer)

    describe "Fractional" $ do
        it "differentiates 1/x to -1/x^2" $ do
            diff (\x -> 1 / x) 2.0 `shouldBeApprox` (-0.25)

        it "divides dual numbers correctly" $ do
            var 6.0 / constant 3.0 `shouldBe` (Dual 2.0 (1.0 / 3.0) :: Dual Double)

    describe "Floating" $ do
        it "differentiates sin to cos" $ do
            diff sin 0.0 `shouldBeApprox` 1.0

        it "differentiates cos to -sin" $ do
            diff cos 0.0 `shouldBeApprox` 0.0

        it "differentiates exp to exp" $ do
            diff exp 1.0 `shouldBeApprox` exp 1.0

        it "differentiates log to 1/x" $ do
            diff log 2.0 `shouldBeApprox` 0.5

        it "differentiates sqrt to 1/(2*sqrt(x))" $ do
            diff sqrt 4.0 `shouldBeApprox` 0.25

        it "differentiates tan to sec^2" $ do
            diff tan 0.0 `shouldBeApprox` 1.0

    describe "chain rule" $ do
        it "differentiates sin(x^2)" $ do
            diff (\x -> sin (x * x)) 1.0 `shouldBeApprox` (2.0 * cos 1.0)

        it "differentiates exp(2x)" $ do
            diff (\x -> exp (2 * x)) 0.0 `shouldBeApprox` 2.0

        it "differentiates log(x^2 + 1)" $ do
            diff (\x -> log (x * x + 1)) 1.0 `shouldBeApprox` 1.0

    describe "higher-order derivatives (diff composition)" $ do
        it "second derivative of x^3 is 6x" $ do
            (diff . diff) (\x -> x * x * x) 2.0 `shouldBeApprox` 12.0

        it "second derivative of x^2 is 2" $ do
            (diff . diff) (\x -> x * x) 5.0 `shouldBeApprox` 2.0

        it "second derivative of sin is -sin" $ do
            (diff . diff) sin 1.0 `shouldBeApprox` (-(sin 1.0))

        it "second derivative of exp is exp" $ do
            (diff . diff) exp 1.0 `shouldBeApprox` exp 1.0

        it "third derivative of x^4 is 24x" $ do
            (diff . diff . diff) (\x -> x * x * x * x) 2.0 `shouldBeApprox` 48.0

        it "third derivative of sin is -cos" $ do
            (diff . diff . diff) sin 0.0 `shouldBeApprox` (-1.0)

        it "diff can evaluate a composed diff at a point" $ do
            diff (diff (\x -> x * x * x)) 2.0 `shouldBeApprox` 12.0

    describe "grad" $ do
        it "computes gradient of x*y" $ do
            grad (\[x, y] -> x * y) [3, 4] `shouldBe` ([4, 3] :: [Integer])

        it "computes gradient of x^2 + y^2" $ do
            grad (\[x, y] -> x * x + y * y) [3, 4] `shouldBe` ([6, 8] :: [Integer])

        it "computes gradient of constants to [0, 0]" $ do
            grad (\_ -> constant 5) [1, 2] `shouldBe` ([0, 0] :: [Integer])

        it "computes gradient of a 3-variable function" $ do
            grad (\[x, y, z] -> x * y * z) [2, 3, 4] `shouldBe` ([12, 8, 6] :: [Integer])
