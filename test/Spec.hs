module Main where

import Test.Hspec

import qualified SpatialSpec
import qualified ObjectSpec
import qualified ShapeSpec

main :: IO ()
main = hspec spec where
  spec = do
    describe "Spatial" SpatialSpec.spec
    describe "Object"   ObjectSpec.spec
    describe "Shapes"    ShapeSpec.spec
