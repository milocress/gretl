module Main where

import Test.Hspec

import qualified DirectionSpec
import qualified ObjectSpec
import qualified SphereSpec

main :: IO ()
main = hspec spec where
  spec = do
    describe "Direction" DirectionSpec.spec
    describe "Object"       ObjectSpec.spec
    describe "Sphere"       SphereSpec.spec
