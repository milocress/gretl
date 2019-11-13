module ShapeSpec (spec) where

import Test.Hspec

import Linear.V3 (V3(..))
import Linear.Metric (normalize)
import Shapes (Sphere(..))
import Object (mindist, distance, direction, marchPath, origin)

import Data.Maybe (isJust)

spec :: Spec
spec = do
  describe "mindist (sphere)" $ do
    it "returns a min distance equal to the distance to the center minus the radius" $
      mindist s b `shouldBe` distance origin b - r

  describe "marchPath (sphere)" $ do
    it "returns Just something when pointed in the general direction of a sphere, but off center" $
      isJust (marchPath (b + offset) dir s mn mx) `shouldBe` True

    where
      s = Sphere r :: Sphere V3 Double
      r = 3 :: Double
      b = V3 5 5 5
      offset = V3 0.1 0.1 0.1
      dir = (normalize $ direction origin b) :: V3 Double
      mn = 1e-6
      mx = 1e3
