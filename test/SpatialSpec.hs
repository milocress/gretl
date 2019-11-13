module SpatialSpec (spec) where

import Test.Hspec

import Spatial
import Linear.V3
import Linear.Metric (norm)


spec :: Spec
spec = describe "extend" $ do
  it "returns a vector of length equal to the argument vector's length plus the argument float" $
    ((pure (norm (extend v d)) :: V3 Double)
     `distance`
     pure (d + norm v) < mn) `shouldBe` True

  it "goes in both directions" $
    ((extend (extend v d) (-d)) `distance` v < mn) `shouldBe` True

  where
    v = V3 1 2 3
    d = 4 :: Double
    mn = 1e-6 -- Magic Number, has to do with the error in floating point precision.
    -- Could be smaller, but it serves to demonstrate a point.
