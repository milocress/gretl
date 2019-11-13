{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , ConstraintKinds
#-}

{-|
Module      : Spatial
Description : General definitions of coordinate systems.

Maintainer  : cress@mit.edu
Stability   : pre-experimental

-}

module Spatial ( Position, distance, origin
               , Direction, extend, magnitude, negated
               , Spatial, move, direction, project
               ) where

import Linear.V3 (V3(..))
import qualified Linear.Metric as L (norm, distance, project)

-- | Abstract notion of a direction.
class Direction d where
  -- | Extend a path in a given direction by a given amount
  extend :: (Floating a) => d a -> a -> d a
  -- | The magnitude of a direction such that this identity holds:
  --
  -- > magnitude . extend a . extend b == magnitude . extend (a + b)
  magnitude :: (Floating a) => d a -> a
  -- | Negates a direction such that
  --
  -- > negated . negated = id
  negated :: (Floating a) => d a -> d a
  negated d = extend d (-2 * magnitude d)

-- | Abstract notion of location.
class Position p where
  -- | The distance between two points
  distance  :: (Floating a) => p a -> p a -> a
  -- | A central point in the coordinate system
  origin    :: (Floating a) => p a

-- | Abstract notion of a coordinate system which
-- couples with a direction system.
class (Position p, Direction d) => Spatial p d where
  -- | Moves a point in a direction and returns the moved point.
  move      :: (Num a) => p a -> d a -> p a
  -- | Gets the direction between two points.
  direction :: (Num a) => p a -> p a -> d a
  -- | \"Snaps\" a point to a given axis, extended from the origin.
  project   :: (Fractional a) => d a -> p a -> p a

instance Direction V3 where
  extend d a = (* ((l + a) / l)) <$> d where
    l = L.norm d
  magnitude = L.norm
  negated = negate

instance Position V3 where
  distance = L.distance
  origin = V3 0 0 0

instance Spatial V3 V3 where
  move = (+)
  direction = (-)
  project = L.project
