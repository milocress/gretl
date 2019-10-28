module ObjectSpec (spec) where

import Test.Hspec

import Object
import Linear.V3
import Linear.Metric (normalize)

import Data.Maybe (fromJust, isJust)

spec :: Spec
spec = do
  describe "mindist" $ do
    it "returns a min distance of cartesian distance for points" $
      mindist a b `shouldBe` distance a b

  describe "marchPath" $ do
    it "returns the point of collision between a vector and an object" $
      (fromJust (marchPath a (normalize $ b - a) b mn mx) `distance` b) < 2 * mn `shouldBe` True

    it "returns Nothing when an object doesn't intersect with a ray" $
      isJust (marchPath a ihat b mn mx) `shouldBe` False

    where
      a = V3 1 2 3
      b = V3 4 5 6 :: V3 Double
      ihat = V3 1 0 0
      mn = 1e-6
      mx = 1e3
